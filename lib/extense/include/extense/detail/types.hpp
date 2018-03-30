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

#include <functional>
#include <map>
#include <optional>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

#include <extense/detail/valuetypebase.hpp>

namespace extense {
struct None {};
inline None none;

struct Int;
struct Float;
struct Bool;
struct Char;
class String;
class Map;
class List;
class Label;
class Scope;

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
    detail::isAnyOf<T, None, Int, Float, Bool, Char, List, String, Map, Label,
                    Scope>;

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
  if (std::is_same_v<String, T>) return "String";
  if (std::is_same_v<List, T>) return "List";
  if (std::is_same_v<Map, T>) return "Map";
  if (std::is_same_v<Label, T>) return "Label";
  if (std::is_same_v<Scope, T>) return "Scope";
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

  operator bool() const noexcept { return value; }
};

inline const Bool Bool::t{true};
inline const Bool Bool::f{false};

struct __attribute__((packed)) Char {
  using ValueType = char;
  ValueType value;

  explicit Char(ValueType v) : value(std::move(v)) {}

  template <typename T,
            std::enable_if_t<implicitlyConvertible<T, Char>> * = nullptr>
  Char(const T &v) {
    *this = convert<T, Char>(std::move(v));
  }

  template <typename T,
            std::enable_if_t<convertible<T, Char> &&
                             !implicitlyConvertible<T, Char>> * = nullptr>
  explicit Char(const T &v) {
    *this = convert<T, Char>(std::move(v));
  }
};

static_assert(sizeof(Char) == sizeof(char));

class String : public detail::ValueTypeBase<String, std::string> {
public:
  using Base::Base;

  Int size() const { return Int{static_cast<Int::ValueType>(value.size())}; }

  const Char &operator[](Int i) const;
  Char &operator[](Int i) {
    return const_cast<Char &>(static_cast<const String &>(*this)[i]);
  }

  const Char &at(Int i) const { return (*this)[i]; }
  Char &at(Int i) {
    return const_cast<Char &>(static_cast<const String &>(*this).at(i));
  }
};

// Can compare directly with a std::string_view to encourage users not to
// create dynamically allocated Strings for comparison operations
inline Bool operator==(const String &a, std::string_view b) {
  return Bool{a.value == b};
}
inline Bool operator==(std::string_view a, const String &b) { return b == a; }

class Value;

template <typename... ValueTypes>
class BasicFlatValue;

using FlatValue = BasicFlatValue<None, Int, Float, Bool, Char, String, List,
                                 Map, Label, Scope>;

namespace detail {
using MapKeyType = BasicFlatValue<Int, Float, Bool, Char, String>;

struct MapCompare {
  bool operator()(const MapKeyType &lhs, const MapKeyType &rhs) const;
};

// Using a map instead of an unordered_map, even though ordering is not needed,
// because unordered_map requires Value to be a complete type
using MapValueType = std::map<MapKeyType, Value, MapCompare>;

// Wraps any value
template <typename T>
struct Wrap {
  T value;
};

template <typename T>
Wrap(T)->Wrap<T>;
} // namespace detail

struct Mapping;

class Map : public detail::ValueTypeBase<Map, detail::MapValueType> {
public:
  using Base::Base;
  using KeyType = detail::MapKeyType;

private:
  // Avoid language weirdness with std::initializer_list
  Map(detail::Wrap<std::initializer_list<ValueType::value_type>> kvps);

  static KeyType constrainToKeyType(const Value &v);
  static KeyType constrainToKeyType(const FlatValue &v);

public:
  // Defined in value.hpp
  Map();

  Map(const Map &) = default;
  Map &operator=(const Map &) = default;
  Map(Map &&) = default;
  Map &operator=(Map &&) = default;

  // Forwards to initializer_list constructor. Defined in value.hpp.
  template <
      typename... Mappings,
      std::enable_if_t<(std::is_same_v<Mapping, Mappings> || ...)> * = nullptr>
  explicit Map(Mappings &&... mappings);

  // Access or create an element. Defined in value.hpp.
  Value &operator[](const KeyType &i);
  Value &operator[](const Value &i);
  Value &operator[](const FlatValue &i);
  template <typename VT>
  Value &operator[](const VT &i);

  // Access an element. Defined in value.hpp
  Value &at(const KeyType &i);
  Value &at(const Value &i);
  Value &at(const FlatValue &i);
  template <typename VT>
  Value &at(const VT &i);

  const Value &at(const KeyType &i) const;
  const Value &at(const Value &i) const;
  const Value &at(const FlatValue &i) const;
  template <typename VT>
  const Value &at(const VT &i) const;

  Int size() const { return Int{static_cast<Int::ValueType>(value.size())}; }
};

namespace {
template <typename...>
struct IsJustList : std::false_type {};

template <>
struct IsJustList<List> : std::true_type {};

template <typename... Ts>
inline constexpr bool isJustList = IsJustList<Ts...>::value;
} // namespace

class List : public detail::ValueTypeBase<List, std::vector<Value>> {
  // Using detail::Wrap to avoid language complications with
  // std::initializer_list
  List(detail::Wrap<std::initializer_list<Value>> values);

public:
  using Base::Base;

  // Defined in value.hpp
  List();

  // Automatically converts to Value to avoid excessive verbosity when
  // constructing. Defined in value.hpp
  template <typename... Elems,
            std::enable_if_t<!isJustList<std::decay_t<Elems>...>> * = nullptr>
  explicit List(Elems &&... elems);

  Value &operator[](Int i) {
    return const_cast<Value &>(static_cast<const List &>(*this)[i]);
  }

  const Value &operator[](Int i) const;
  const Value &at(Int i) const;

  List operator[](const List &i) const;
  List at(const List &i) const { return (*this)[i]; }

  Value at(const Value &i) const;

  Int size() const { return Int{static_cast<Int::ValueType>(value.size())}; }
};

class ExprList;

class Label {
  int index;
  std::string name_;

  Label(int index_, std::string name) : index(index_), name_(std::move(name)) {}
  friend class ExprList;

public:
  const std::string &name() const { return name_; }
  void rename(std::string newName) { name_ = std::move(newName); }
};

class Scope {
public:
  using FunctionSignature = Value(Scope &, const Value &);
  using Function = std::function<FunctionSignature>;

private:
  Function func;
  Scope *outer_;
  // Using map as it does not require the definition of Value, and iterators are
  // not invalidated unless the iterator's element is removed.
  std::map<std::string, Value> identifiers;

  Value call(const Value &v);

public:
  Scope(Function f, Scope *outer = nullptr)
      : func(std::move(f)), outer_(outer) {}

  const Scope *outer() const { return outer_; }
  Scope *outer() { return outer_; }

  const Function &function() const { return func; }
  Function &function() { return func; }

  const Value &getIdentifier(const std::string &name) const;
  Value &getIdentifier(const std::string &name) {
    return const_cast<Value &>(
        static_cast<const Scope *>(this)->getIdentifier(name));
  }

  const Value *findIdentifier(const std::string &name) const;
  Value *findIdentifier(const std::string &name) {
    return const_cast<Value *>(
        static_cast<const Scope *>(this)->findIdentifier(name));
  }

  // Creates an identifier in the current scope, even if a variable with the
  // same name is already set in an outer scope
  Value &createIdentifier(const std::string &name);

  // Will automatically create an identifier with that name if it isn't
  // already present in the scope or outer scopes
  Value &createOrGetIdentifier(const std::string &name);

  void clearIdentifiers() { identifiers.clear(); }

  // Defined in value.hpp
  Value operator()();

  // Convenience function. If there is one argument, the function is called with
  // that argument (as a Value). Otherwise, a List is constructed from the
  // arguments and the function is called with the List. This function is
  // defined in value.hpp since the definition requires Value to be defined.
  template <typename... Values>
  Value operator()(Values &&... values);
};

namespace literals {
inline Int operator"" _ei(unsigned long long v) {
  return Int{static_cast<Int::ValueType>(v)};
}

inline Float operator"" _ef(long double v) {
  return Float{static_cast<Float::ValueType>(v)};
}

inline Char operator"" _ec(char v) { return Char{v}; }

inline String operator"" _es(const char *str, std::size_t len) {
  return String{std::string(str, len)};
}
} // namespace literals

std::ostream &operator<<(std::ostream &, const Value &);

inline std::ostream &operator<<(std::ostream &os, None) {
  os << "None";
  return os;
}

inline std::ostream &operator<<(std::ostream &os, Int v) {
  os << v.value;
  return os;
}

inline std::ostream &operator<<(std::ostream &os, Float v) {
  os << v.value;
  return os;
}

inline std::ostream &operator<<(std::ostream &os, Bool v) {
  os << (v.value ? "true" : "false");
  return os;
}

inline std::ostream &operator<<(std::ostream &os, Char v) {
  os << v.value;
  return os;
}

inline std::ostream &operator<<(std::ostream &os, const String &v) {
  for (auto c : v.value) os << c;
  return os;
}

std::ostream &operator<<(std::ostream &, const Map &);
std::ostream &operator<<(std::ostream &, const List &);

// TODO: Print AST for scope
inline std::ostream &operator<<(std::ostream &os, const Scope &) {
  os << "<Scope>";
  return os;
}

inline std::ostream &operator<<(std::ostream &os, const Label &v) {
  os << "<Label with name '" << v.name() << " '>";
  return os;
}

namespace detail {
/*
 * It is perfectly fine to convert from any type which implements operator<<
 * to a String.
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
