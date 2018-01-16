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

#ifndef _LIB_EXTENSE__VALUE_HPP
#define _LIB_EXTENSE__VALUE_HPP

#include <cstdint>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>

#include <extense/detail/operations.hpp>

namespace extense {
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
    BasicFlatValue<None, Int, Float, Bool, Char, String, List, Set, Scope>;

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
  // Initializes with first ValueType
  BasicFlatValue() = default;

  template <typename T>
  explicit BasicFlatValue(T v) : data(std::move(v)) {
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

  template <typename T, typename TValue>
  friend const T &get(const TValue &v);

  // To implement comparison the internal variant data is needed
  friend struct detail::SetCompare;

  friend Bool ops::equal<>(const BasicFlatValue &, const BasicFlatValue &);

  const Data &internalVariant() const { return data; }
  Data &internalVariant() { return data; }
};

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
    detail::isAnyOf<VT, String, List, Set, Scope>;

class Value {
  using Data = std::variant<FlatValue, Reference>;
  Data data;

  template <typename ValueType>
  friend struct detail::InternalData;

public:
  // Initializes with None
  Value() = default;

  template <typename T>
  explicit Value(T v) {
    if constexpr (std::is_same_v<T, Reference>)
      data = v;
    else if constexpr (isReferencedOnCopy<T>) {
      auto newVal = makeReference(std::move(v));
      data = newVal;
    } else
      data = FlatValue(std::move(v));
  }

  template <typename T>
  Value &operator=(T v) {
    if constexpr (std::is_same_v<T, Reference>)
      data = std::move(v);
    else
      *this = Value(std::move(v));
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
      return flatten().is<T>();
  }

  template <typename T, typename TValue>
  friend const T &get(const TValue &v);

  friend std::ostream &operator<<(std::ostream &os, const Value &v);

  friend Bool ops::equal(const Value &, const Value &);
};

namespace {
// A possible alternative contained in the TValue, for use in determining the
// return type of a visitor
template <typename TValue>
using TestInput = std::conditional_t<
    std::is_same_v<TValue, Value>,
    // If the first type is a Value the function should be
    // able to handle a None object
    None,
    // Otherwise, it should be able to handle the first
    // possible type in the BasicFlatValue
    decltype(std::get<0>(
        std::declval<typename detail::InternalData<TValue>::Type>()))>;

template <typename TValue>
const auto &tryFlatten(const TValue &v) {
  if constexpr (std::is_same_v<TValue, Value>)
    return v.flatten();
  else
    return v;
}

template <typename TValue>
auto &tryFlatten(TValue &v) {
  if constexpr (std::is_same_v<TValue, Value>)
    return v.flatten();
  else
    return v;
}
} // namespace

// Visits a Value, similarly to std::visit with a variant, but automatically
// handles referenced types
template <typename Visitor, typename... Values>
decltype(auto) visit(Visitor visitor, Values &&... values) {
  using ResultType =
      decltype(visitor(std::declval<TestInput<std::decay_t<Values>>>()...));

  return std::visit(
      [&visitor](auto &&... args) -> ResultType {
        return visitor(std::forward<decltype(args)>(args)...);
      },
      (tryFlatten(std::forward<decltype(values)>(values))
           .internalVariant())...);
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

namespace ops {
template <typename... ValueTypes>
Bool equal(const BasicFlatValue<ValueTypes...> &a,
           const BasicFlatValue<ValueTypes...> &b) {
  // We want a Float and Int with the same values (e.g. 1.0 and 1) to compare
  // equal. A variant's default operator== will not do this, and so this special
  // case is handled manually.
  if ((a.template is<Float>() && b.template is<Int>()) ||
      (a.template is<Int>() && b.template is<Float>()))
    return as<Float>(a) == as<Float>(b);

  return Bool{a.data == b.data};
}

inline Bool equal(const Value &a, const Value &b) {
  // We compare the flattened values, because a Reference to an Int with value 1
  // should compare equal to an Int with value 1
  return Bool{a.flatten() == b.flatten()};
}
} // namespace ops

template <typename ValueType>
class LiteralShow;

template <typename ValueType>
std::ostream &operator<<(std::ostream &os,
                         const extense::LiteralShow<ValueType> &v);

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

template <typename ValueType>
std::ostream &operator<<(std::ostream &os, const LiteralShow<ValueType> &v) {
  const auto &value = v.getContained();
  // TODO: Automatically escape characters (replace an actual newline
  // character with \n)
  if constexpr (std::is_same_v<ValueType, Char>)
    os << '`' << value;
  else if constexpr (std::is_same_v<ValueType, String>)
    os << '"' << value << '"';
  else if constexpr (std::is_same_v<ValueType, Value> ||
                     detail::isBasicFlatValue<ValueType>) {
    visit([&os](const auto &data) { os << LiteralShow{data}; }, value);
  } else
    os << value;
  return os;
}

template <typename... ValueTypes>
std::ostream &operator<<(std::ostream &os,
                         const BasicFlatValue<ValueTypes...> &v) {
  visit([&os](const auto &arg) { os << arg; }, v);
  return os;
}

std::ostream &operator<<(std::ostream &, const Reference &);

std::ostream &operator<<(std::ostream &os, const Value &v);

inline const Value noneValue{none};

template <typename... Values>
Value Scope::operator()(Values &&... values) {
  if constexpr (sizeof...(Values) == 1)
    return func(*this, static_cast<Value>(std::forward<Values>(values))...);
  else
    return func(*this, Value{List{std::forward<Values>(values)...}});
}

struct Mapping {
  // Converts value types to the necessary types for convenience
  template <typename K, typename V>
  Mapping(const K &k, const V &v)
      : key(detail::SetKeyType(k)), value(Value(v)) {}

  std::pair<const detail::SetKeyType, Value> toPair() {
    return std::make_pair<const detail::SetKeyType, Value>(std::move(key),
                                                           std::move(value));
  }

private:
  const detail::SetKeyType key;
  Value value;
};

inline List::List() : Base(ValueType()) {}

inline Set::Set() : Base(ValueType()) {}

template <typename... Elems>
List::List(Elems &&... elems)
    : List(detail::Wrap{std::initializer_list<Value>{
          static_cast<Value>(std::forward<Elems>(elems))...}}) {}

template <typename... Mappings,
          std::enable_if_t<(std::is_same_v<Mapping, Mappings> || ...)> *>
Set::Set(Mappings &&... mappings)
    : Set(detail::Wrap{std::initializer_list<ValueType::value_type>(
          {(std::forward<Mappings>(mappings).toPair())...})}) {}

template <typename VT>
const Value &Set::operator[](const VT &i) const {
  static_assert(KeyType::supportsType<VT>,
                "Cannot index with an object of the given type");
  return (*this)[KeyType{i}];
}
template <typename VT>
Value &Set::operator[](const VT &i) {
  static_assert(KeyType::supportsType<VT>,
                "Cannot index with an object of the given type");
  return (*this)[KeyType{i}];
}
} // namespace extense

#endif // _LIB_EXTENSE__VALUE_HPP
