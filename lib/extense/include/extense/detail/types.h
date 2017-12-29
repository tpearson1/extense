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

#ifndef _LIB_EXTENSE_DETAIL__TYPES_H
#define _LIB_EXTENSE_DETAIL__TYPES_H

#include <map>
#include <ostream>
#include <sstream>
#include <string>
#include <variant>
#include <vector>

#include <extense/detail/valuetypebase.h>

namespace extense {
struct None {
  bool operator==(None) const { return true; }
  bool operator!=(None) const { return false; }
  bool operator<=(None) const { return true; }
  bool operator>=(None) const { return true; }
  bool operator<(None) const { return false; }
  bool operator>(None) const { return false; }
};

struct Int;
struct Float;
struct Bool;
struct Char;
struct String;
struct Set;
struct List;

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
} // namespace detail

struct Int : public detail::ValueTypeBase<Int, std::int64_t> {
  using Base::Base;
  explicit Int(std::int32_t v) : Base(static_cast<ValueType>(v)) {}
};

struct Float : public detail::ValueTypeBase<Float, double> {
  using Base::Base;
  explicit Float(float v) : Base(static_cast<ValueType>(v)) {}
};

struct Bool : public detail::ValueTypeBase<Bool, bool> {
  using Base::Base;
};

struct Char : public detail::ValueTypeBase<Char, char> {
  using Base::Base;
};

struct String : public detail::ValueTypeBase<String, std::string> {
  using Base::Base;
};

class Value;

template <typename... ValueTypes>
class BasicFlatValue;

namespace detail {
using SetKeyType = BasicFlatValue<Int, Float, Bool, Char, String>;
// Using a map instead of an unordered_map, even though ordering is not needed,
// because unordered_map requires Value to be a complete type
using SetValueType = std::map<SetKeyType, Value>;
} // namespace detail

struct Set : detail::ValueTypeBase<Set, detail::SetValueType> {
  using Base::Base;
  using KeyType = detail::SetKeyType;

  Set(std::initializer_list<ValueType::value_type> kvps);
};

struct List : detail::ValueTypeBase<List, std::vector<Value>> {
  using Base::Base;

  List(std::initializer_list<Value> values)
      : Base{ValueType{std::move(values)}} {}
};
} // namespace extense

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

namespace extense::detail {
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
} // namespace extense::detail

#endif // _LIB_EXTENSE_DETAIL__TYPES_H
