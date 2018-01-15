/*
-------------------------------------------------------------------------------
This file is part of Extense
-------------------------------------------------------------------------------
Copyright (c) 2018 Thomas Pearson

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-------------------------------------------------------------------------------
*/

#ifndef _LIB_EXTENSE_DETAIL__TYPES_HPP
#define _LIB_EXTENSE_DETAIL__TYPES_HPP

#include <map>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

#include <extense/detail/valuetypebase.hpp>

namespace extense {
struct None {};
struct Int;
struct Float;
struct Bool;
struct Char;
class String;
class Set;
class List;

class Reference;

namespace detail {
template <typename T, typename... Ts>
inline constexpr bool isAnyOf = (std::is_same_v<T, Ts> || ...);

template <typename T>
struct Convert<T, String>;

template <typename T>
struct Convert<T, List>;

template <>
struct Convert<Float, Int>;

template <>
struct Convert<Int, Float>;

template <>
struct Convert<Char, String>;

// Whether or not T is a type which could be stored in a FlatValue e.g. Bool,
// String
template <typename T>
inline constexpr bool isFlatValueType =
    detail::isAnyOf<T, None, Int, Float, Bool, Char, List, String, Set>;

// Whether or not T is a valid type for a Value - either a FlatValue type or a
// Reference
template <typename T>
inline constexpr bool isValueType =
    isFlatValueType<T> || std::is_same_v<T, Reference>;

template <typename T>
constexpr std::string_view typeAsString() {
  static_assert(detail::isValueType<T>,
                "Invalid type passed to 'extense::typeAsString'");
  if (std::is_same_v<None, T>) return "None";
  if (std::is_same_v<Int, T>) return "Int";
  if (std::is_same_v<Float, T>) return "Float";
  if (std::is_same_v<Bool, T>) return "Bool";
  if (std::is_same_v<Char, T>) return "Char";
  if (std::is_same_v<List, T>) return "List";
  if (std::is_same_v<String, T>) return "String";
  if (std::is_same_v<Set, T>) return "Set";
  if (std::is_same_v<Reference, T>) return "Reference";
}
} // namespace detail

template <typename ValueType>
inline constexpr std::string_view
    typeAsString = detail::typeAsString<ValueType>();

struct Int : detail::ValueTypeBase<Int, std::int64_t> {
  using Base::Base;
  explicit Int(std::int32_t v) : Base(static_cast<ValueType>(v)) {}
};

struct Float : detail::ValueTypeBase<Float, double> {
  using Base::Base;
  explicit Float(float v) : Base(static_cast<ValueType>(v)) {}
};

struct Bool : detail::ValueTypeBase<Bool, bool> {
  using Base::Base;

  static const Bool t;
  static const Bool f;

  operator bool() const { return value; }
};

inline const Bool Bool::t{true};
inline const Bool Bool::f{false};

struct Char : detail::ValueTypeBase<Char, char> {
  using Base::Base;
};

class String : public detail::ValueTypeBase<String, std::string> {
public:
  using Base::Base;
};

class Value;

template <typename... ValueTypes>
class BasicFlatValue;

namespace detail {
using SetKeyType = BasicFlatValue<Int, Float, Bool, Char, String>;

struct SetCompare {
  bool operator()(const SetKeyType &lhs, const SetKeyType &rhs) const;
};

// Using a map instead of an unordered_map, even though ordering is not needed,
// because unordered_map requires Value to be a complete type
using SetValueType = std::map<SetKeyType, Value, SetCompare>;
} // namespace detail

class Set : public detail::ValueTypeBase<Set, detail::SetValueType> {
public:
  using Base::Base;
  using KeyType = detail::SetKeyType;

  Set(std::initializer_list<ValueType::value_type> kvps);

  // Access an element
  const Value &operator[](const KeyType &i) const;
  Value &operator[](const KeyType &i);

  // Returns a reference to the element associated with the given key, creating
  // the element if not present
  Value &insertOrAccess(const KeyType &i);
};

class List : public detail::ValueTypeBase<List, std::vector<Value>> {
public:
  using Base::Base;

  List();

  List(std::initializer_list<Value> values);

  Value &operator[](Int i) {
    return const_cast<Value &>(static_cast<const List &>(*this)[i]);
  }

  const Value &operator[](Int i) const;

  List operator[](const List &i) const;
};

std::ostream &operator<<(std::ostream &, const extense::Value &);

inline std::ostream &operator<<(std::ostream &os, extense::None) {
  os << "None";
  return os;
}

inline std::ostream &operator<<(std::ostream &os, const extense::Int &v) {
  os << v.value;
  return os;
}

inline std::ostream &operator<<(std::ostream &os, const extense::Float &v) {
  os << v.value;
  return os;
}

inline std::ostream &operator<<(std::ostream &os, const extense::Bool &v) {
  os << (v.value ? "true" : "false");
  return os;
}

inline std::ostream &operator<<(std::ostream &os, const extense::Char &v) {
  os << v.value;
  return os;
}

inline std::ostream &operator<<(std::ostream &os, const extense::String &v) {
  os << v.value;
  return os;
}

std::ostream &operator<<(std::ostream &, const extense::Set &);
std::ostream &operator<<(std::ostream &, const extense::List &);

namespace detail {
/*
 * It is perfectly fine to convert from any type which implements operator<< to
 * a String.
 */
template <typename T>
struct Convert<T, String> {
  String operator()(const T &from) const {
    std::ostringstream os;
    os << from;
    return String{os.str()};
  }
};

template <typename T>
struct Convert<T, List> {
  List operator()(T from) { return List{List::ValueType{std::move(from)}}; }
};

template <>
struct Convert<Float, Int> {
  Int operator()(Float from) {
    return Int{static_cast<Int::ValueType>(from.value)};
  }
};

template <>
struct Convert<Int, Float> {
  struct Implicitly {};
  Float operator()(Int from) {
    return Float{static_cast<Float::ValueType>(from.value)};
  }
};

template <>
struct Convert<Char, String> {
  struct Implicitly {};
  String operator()(Char c) { return String{std::string(1, c.value)}; }
};
} // namespace detail
} // namespace extense

#endif // _LIB_EXTENSE_DETAIL__TYPES_HPP
