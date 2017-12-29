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

#ifndef _LIB_EXTENSE__VALUE_H
#define _LIB_EXTENSE__VALUE_H

#include <cstdint>
#include <memory>
#include <tuple>
#include <type_traits>

#include <extense/detail/types.h>

namespace extense {
class Reference;

namespace detail {
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

/*
 * Exception thrown when an operation is found to be invalid, such as the
 * addition of two Values with contained type Char.
 */
class InvalidOperation : public std::exception {
  std::string v1, v2;

public:
  InvalidOperation(std::string firstType, std::string secondType)
      : v1(std::move(firstType)), v2(std::move(secondType)) {}

  template <typename V1, typename V2>
  static InvalidOperation Create() {
    return InvalidOperation{typeAsString<V1>, typeAsString<V2>};
  }

  std::string firstType() const { return v1; }
  std::string secondType() const { return v2; }

  virtual const char *what() const noexcept override {
    return ("Invalid operand Values: '" + v1 + "' and '" + v2 + '\'').c_str();
  }
};

/*
 * Exception thrown when a conversion (either implicit or explicit) couldn't
 * be performed.
 */
class InvalidConversion : public std::exception {
  std::string fromT, toT;
  bool implicit;

public:
  InvalidConversion(std::string from, std::string to, bool implicitly)
      : fromT(std::move(from)), toT(std::move(to)), implicit(implicitly) {}

  template <typename V1, typename V2>
  static InvalidConversion Create(bool implicitly) {
    return InvalidConversion{std::string{typeAsString<V1>},
                             std::string{typeAsString<V2>}, implicitly};
  }

  std::string from() const { return fromT; }
  std::string to() const { return toT; }

  bool attemptedImplicit() const { return implicit; }

  virtual const char *what() const noexcept override;
};

template <typename From, typename To>
To tryConvertImplicitly(const From &f) {
  if constexpr (implicitlyConvertible<From, To>) return convert<From, To>(f);
  throw InvalidConversion::Create<From, To>(true);
}

template <typename From, typename To>
To tryConvert(const From &f) {
  if constexpr (convertible<From, To>) return convert<From, To>(f);
  throw InvalidConversion::Create<From, To>(false);
}

using FlatValue =
    BasicFlatValue<None, Int, Float, Bool, Char, String, List, Set>;

namespace detail {
// Used to expose the internal data representation of a BasicFlatValue/Value to
// template meta-programming used by the visit function
template <typename ValueType>
struct InternalData {
  // Base case for Value
  using Type = std::variant<FlatValue, Reference>;
};

template <typename... ValueTypes>
struct InternalData<BasicFlatValue<ValueTypes...>> {
  using Type = std::variant<ValueTypes...>;
};
} // namespace detail

// A value which cannot be a reference
template <typename... ValueTypes>
class BasicFlatValue {
  using Data = std::variant<ValueTypes...>;
  Data data;

public:
  // Initializes with None
  BasicFlatValue() = default;

  template <typename T>
  BasicFlatValue(T v) : data(std::move(v)) {
    static_assert(supportsType<T>, "Cannot construct an instance of this "
                                   "template instantiation of a BasicFlatValue "
                                   "with a value of the given type");
  }

  template <typename T>
  inline static constexpr bool supportsType = detail::isAnyOf<T, ValueTypes...>;

  template <typename T>
  bool is() const {
    static_assert(supportsType<T>,
                  "A FlatValue cannot possibly be of the given "
                  "type T");
    return std::holds_alternative<T>(data);
  }

  template <typename Visitor, typename... Values>
  friend auto visit(Visitor, Values &&...);

  template <typename T, typename TValue>
  friend const T &get(const TValue &v);

  // Each comparison operation will only be available if the ValueTypes for
  // the template class all support the operation
  bool operator==(const BasicFlatValue &other) const {
    return data == other.data;
  }
  bool operator!=(const BasicFlatValue &other) const {
    return data != other.data;
  }

  bool operator<(const BasicFlatValue &other) const {
    return data < other.data;
  }
  bool operator<=(const BasicFlatValue &other) const {
    return data <= other.data;
  }

  bool operator>(const BasicFlatValue &other) const {
    return data > other.data;
  }
  bool operator>=(const BasicFlatValue &other) const {
    return data >= other.data;
  }
};

namespace detail {
template <typename T>
struct IsBasicFlatValue : std::false_type {};

template <typename... ValueTypes>
struct IsBasicFlatValue<BasicFlatValue<ValueTypes...>> : std::true_type {};

template <typename T>
inline constexpr bool isBasicFlatValue = IsBasicFlatValue<T>::value;
} // namespace detail

class Reference {
  std::shared_ptr<FlatValue> value;

public:
  Reference() = default;

  explicit Reference(std::shared_ptr<FlatValue> v) : value(std::move(v)) {}

  explicit Reference(Value &v);
  explicit Reference(FlatValue v);

  FlatValue &operator*() { return *value; }
  const FlatValue &operator*() const { return *value; }

  FlatValue *operator->() { return value.get(); }
  const FlatValue *operator->() const { return value.get(); }

  bool operator==(const Reference &other) const { return value == other.value; }
  bool operator!=(const Reference &other) const { return value != other.value; }
};

template <typename... Args>
Reference makeReference(Args &&... args) {
  return Reference{std::make_shared<FlatValue>(std::forward<Args>(args)...)};
}

template <typename VT>
inline constexpr bool isReferencedOnCopy =
    detail::isAnyOf<VT, String, List, Set>;

class Value {
  using Data = std::variant<FlatValue, Reference>;
  Data data;

  template <typename ValueType>
  friend struct detail::InternalData;

public:
  // Initializes with None
  Value() = default;

  template <typename T>
  Value(T v) {
    if constexpr (std::is_same_v<T, Reference>)
      data = v;
    else if constexpr (isReferencedOnCopy<T>) {
      auto newVal = makeReference(std::move(v));
      data = newVal;
    } else
      data = FlatValue{std::move(v)};
  }

  template <typename T>
  Value &operator=(T v) {
    *this = Value{std::move(v)};
    return *this;
  }

  template <typename T>
  inline static constexpr bool supportsType = detail::isValueType<T>;

  Value(const Value &) = default;
  Value(Value &&) = default;

  Value &operator=(const Value &) = default;
  Value &operator=(Value &&) = default;

  std::string typeAsString() const;

  const FlatValue &flatten() const {
    if (is<Reference>()) return *std::get<Reference>(data);
    return std::get<FlatValue>(data);
  }

  FlatValue &flatten() {
    return const_cast<FlatValue &>(static_cast<const Value *>(this)->flatten());
  }

  template <typename T>
  bool is() const {
    static_assert(supportsType<T>, "A Value cannot possibly be of the given "
                                   "type T");
    if constexpr (std::is_same_v<T, Reference>)
      return std::holds_alternative<Reference>(data);
    else
      return std::get<FlatValue>(data).is<T>();
  }

  template <typename Visitor, typename... Values>
  friend auto visit(Visitor, Values &&...);

  template <typename T, typename TValue>
  friend const T &get(const TValue &v);

  friend std::ostream &operator<<(std::ostream &os, const extense::Value &v) {
    std::visit([&os](const auto &arg) { os << arg; }, v.data);
    return os;
  }

  bool operator==(const Value &other) const { return data == other.data; }
  bool operator!=(const Value &other) const { return data != other.data; }
};

namespace detail {
// A possible alternative contained in the TValue, for use in determining the
// return type of a visitor
template <typename TValue>
using TestInput = std::conditional_t<
    std::is_same_v<TValue, Value>,
    // If the first type is a Value the function should be
    // able to handle a None type
    None,
    // Otherwise, it should be able to handle the first
    // possible type in the BasicFlatValue
    decltype(std::get<0>(
        std::declval<typename detail::InternalData<TValue>::Type>()))>;
} // namespace detail

// Visits a Value, similarly to std::visit with a variant, but automatically
// handles referenced types
template <typename Visitor, typename... Values>
auto visit(Visitor visitor, Values &&... values) {
  auto getFlattenedData = [](auto &value) -> auto & {
    using TValue = std::decay_t<decltype(value)>;
    if constexpr (std::is_same_v<TValue, Value>)
      return value.flatten().data;
    else // Is BasicFlatValue
      return value.data;
  };

  Value v;
  auto &fd = getFlattenedData(v);
  static_assert(std::is_same_v<
                std::decay_t<decltype(fd)>,
                std::variant<None, Int, Float, Bool, Char, String, List, Set>>);

  using ResultType = decltype(
      visitor(std::declval<detail::TestInput<std::decay_t<Values>>>()...));

  return std::visit(
      [&visitor](auto &&... args) -> ResultType {
        return visitor(std::forward<decltype(args)>(args)...);
      },
      getFlattenedData(std::forward<decltype(values)>(values))...);
}

// Get's a reference the contained value, without making any conversions.
// REVIEW: Wrap into custom exception on failure?
template <typename T, typename TValue>
const T &get(const TValue &v) {
  static_assert(TValue::template supportsType<T>,
                "Invalid type passed to 'extense::get'");
  if constexpr (std::is_same_v<T, Reference>) {
    // If we flatten we will not be able to get the Reference
    return std::get<Reference>(v.data);
  } else {
    if constexpr (std::is_same_v<TValue, Value>)
      return std::get<T>(v.flatten().data);
    else
      return std::get<T>(v.data);
  }
}

template <typename T, typename TValue>
T &get(TValue &v) {
  return const_cast<T &>(get<T>(static_cast<const TValue &>(v)));
}

inline Reference::Reference(Value &v) {
  if (v.is<Reference>()) {
    *this = get<Reference>(v);
    return;
  }

  value = std::make_shared<FlatValue>(v.flatten());
  v = *this;
}

inline Reference::Reference(FlatValue v) {
  value = std::make_shared<FlatValue>(std::move(v));
}

// Will do an implicit conversion (e.g. Int to Float) if necessary.
template <typename T, typename TValue>
T as(const TValue &v) {
  return visit(
      [](auto &&val) -> T {
        using VT = std::decay_t<decltype(val)>;
        return tryConvertImplicitly<VT, T>(val);
      },
      v);
}

template <typename T, typename TValue>
T convertedTo(const TValue &v) {
  return visit(
      [](auto &&val) -> T {
        using VT = std::decay_t<decltype(val)>;
        return tryConvert<VT, T>(val);
      },
      v);
}

template <typename ValueType>
class LiteralShow;
} // namespace extense

template <typename ValueType>
std::ostream &operator<<(std::ostream &os,
                         const extense::LiteralShow<ValueType> &v);

namespace extense {
// Helper type to show a Value type (e.g. String) literally, which means
// that it is shown with the syntax that would be used to express it in
// code. Is used as follows: os << LiteralShow{String{"Example"}};
template <typename ValueType>
class LiteralShow {
  const ValueType &vt;

public:
  LiteralShow(const ValueType &valueType) : vt{valueType} {}

  const ValueType &getContained() const { return vt; }

  friend std::ostream &operator<<<>(std::ostream &os, const LiteralShow &v);
};

template <typename ValueType>
LiteralShow(const ValueType &)->LiteralShow<ValueType>;
} // namespace extense

template <typename ValueType>
std::ostream &operator<<(std::ostream &os,
                         const extense::LiteralShow<ValueType> &v) {
  const auto &value = v.getContained();
  // TODO: Automatically escape characters (replace an actual newline
  // character with \n)
  if constexpr (std::is_same_v<ValueType, extense::Char>)
    os << '`' << value;
  else if constexpr (std::is_same_v<ValueType, extense::String>)
    os << '"' << value << '"';
  else if constexpr (std::is_same_v<ValueType, extense::Value> ||
                     extense::detail::isBasicFlatValue<ValueType>) {
    extense::visit(
        [&os](const auto &data) { os << extense::LiteralShow{data}; }, value);
  } else
    os << value;
  return os;
}

template <typename... ValueTypes>
std::ostream &operator<<(std::ostream &os,
                         const extense::BasicFlatValue<ValueTypes...> &v) {
  extense::visit([&os](const auto &arg) { os << arg; }, v);
  return os;
}

std::ostream &operator<<(std::ostream &, const extense::Value &);
std::ostream &operator<<(std::ostream &, const extense::Reference &);

#endif // _LIB_EXTENSE__VALUE_H
