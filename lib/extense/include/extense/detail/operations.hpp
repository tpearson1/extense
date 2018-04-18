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

#ifndef _LIB_EXTENSE_DETAIL__OPERATIONS_HPP
#define _LIB_EXTENSE_DETAIL__OPERATIONS_HPP

#include <cmath>

#include <extense/detail/types.hpp>
#include <extense/exception.hpp>

namespace extense {
/*
 * Exception thrown when a binary operation is found to be invalid, such as the
 * addition of two Values with contained type Char.
 */
class InvalidBinaryOperation : public Exception {
  std::string v1_, v2_, reason_;

  inline static constexpr auto defaultReason =
      "Operation not defined on instances of the given types";

  std::string build(std::string v1, std::string v2, std::string reason) {
    v1_ = std::move(v1);
    v2_ = std::move(v2);
    reason_ = std::move(reason);
    return "Invalid binary operation with types '" + v1_ + "' + '" + v2_ +
           "': " + reason_;
  }

public:
  InvalidBinaryOperation(std::string firstType, std::string secondType,
                         std::string reason = defaultReason)
      : Exception("") {
    setError(
        build(std::move(firstType), std::move(secondType), std::move(reason)));
    setType("InvalidBinaryOperation");
  }

  InvalidBinaryOperation(const Value &a, const Value &b,
                         std::string reason = defaultReason);

  template <typename V1, typename V2>
  static InvalidBinaryOperation Create(std::string reason = defaultReason) {
    return InvalidBinaryOperation{std::string{typeAsString<V1>},
                                  std::string{typeAsString<V2>},
                                  std::move(reason)};
  }

  const std::string &firstType() const noexcept { return v1_; }
  const std::string &secondType() const noexcept { return v2_; }
  const std::string &message() const noexcept { return reason_; }
};

/*
 * Exception thrown when a binary operation is found to be invalid, such as the
 * addition of two Values with contained type Char.
 */
class InvalidUnaryOperation : public Exception {
  std::string v1_, v2_, reason_;

  inline static constexpr auto defaultReason =
      "Operation not defined on an instance of the given type";

  std::string build(std::string v1, std::string reason) {
    v1_ = std::move(v1);
    reason_ = std::move(reason);
    return "Invalid unary operation with type '" + v1_ + "': " + reason_;
  }

public:
  InvalidUnaryOperation(std::string firstType,
                        std::string reason = defaultReason)
      : Exception("") {
    setError(build(std::move(firstType), std::move(reason)));
    setType("InvalidUnaryOperation");
  }

  InvalidUnaryOperation(const Value &a, std::string reason = defaultReason);

  template <typename V1>
  static InvalidUnaryOperation Create(std::string reason = defaultReason) {
    return InvalidUnaryOperation{std::string{typeAsString<V1>},
                                 std::move(reason)};
  }

  const std::string &firstType() const noexcept { return v1_; }
  const std::string &message() const noexcept { return reason_; }
};

class OperatorOverloadError : public Exception {
public:
  explicit OperatorOverloadError(
      std::string error =
          "Operator not defined for given Map/UserObject, or operator "
          "changed type of first argument")
      : Exception(std::move(error)) {
    setType("OperatorOverloadError");
  }
};

namespace detail {
template <typename T>
struct IsBasicFlatValue : public std::false_type {};

template <typename... VTs>
struct IsBasicFlatValue<BasicFlatValue<VTs...>> : public std::true_type {};

template <typename T>
inline constexpr bool isBasicFlatValue = IsBasicFlatValue<T>::value;

template <typename... VTs>
using enableValidOpArgs =
    std::enable_if_t<((isValueType<VTs> || std::is_same_v<VTs, Value> ||
                       isBasicFlatValue<VTs>)&&...)>;
} // namespace detail

namespace ops {
// Increment/decrement operators (only for the API; not accessible within the
// language)
inline Int &inc(Int &a) {
  ++a.value;
  return a;
}
inline Int &dec(Int &a) {
  --a.value;
  return a;
}

inline Float &inc(Float &a) {
  ++a.value;
  return a;
}
inline Float &dec(Float &a) {
  --a.value;
  return a;
}

// +
Value add(const Value &a, const Value &b);

inline Int add(Int a, Int b) { return Int{a.value + b.value}; }
inline Float add(Float a, Float b) { return Float{a.value + b.value}; }
inline String add(const String &a, const String &b) {
  return String{a.value + b.value};
}
List add(List a, const List &b);

// +x
Value unaryPlus(const Value &a);
inline Int unaryPlus(Int a) { return a; }
inline Float unaryPlus(Float a) { return a; }

// -
Value sub(const Value &a, const Value &b);

inline Int sub(Int a, Int b) { return Int{a.value - b.value}; }
inline Float sub(Float a, Float b) { return Float{a.value - b.value}; }

// -x
Value unaryMinus(const Value &a);
inline Int unaryMinus(Int a) { return Int{-a.value}; }
inline Float unaryMinus(Float a) { return Float{-a.value}; }

// *
Value mul(const Value &a, const Value &b);

inline Int mul(Int a, Int b) { return Int{a.value * b.value}; }
inline Float mul(Float a, Float b) { return Float{a.value * b.value}; }
String mul(String a, Int times);
List mul(List a, Int times);

/*
 * Exception thrown when input to a mathematical function is invalid.
 */
class DomainError : public Exception {
public:
  explicit DomainError(std::string error) : Exception(std::move(error)) {
    setType("DomainError");
  }
};

class DivisionByZeroError : public Exception {
public:
  explicit DivisionByZeroError(std::string error = "Division by zero")
      : Exception(std::move(error)) {
    setType("DivisionByZeroError");
  }
};

namespace {
template <typename ValueType>
inline static void checkDivision(ValueType b) {
  if (b.value != 0) return;
  throw DivisionByZeroError{};
}
} // namespace

// /
Value div(const Value &a, const Value &b);

inline Float div(Float a, Float b) {
  checkDivision(b);
  return Float{a.value / b.value};
}

// %
Value mod(const Value &a, const Value &b);

inline Int mod(Int a, Int b) {
  checkDivision(b);
  return Int{a.value % b.value};
}

inline Float mod(Float a, Float b) {
  checkDivision(b);
  return Float{std::fmod(a.value, b.value)};
}

// //
Value floorDiv(const Value &a, const Value &b);

inline Int floorDiv(Int a, Int b) {
  checkDivision(b);
  auto result = std::floor(static_cast<Float::ValueType>(a.value) /
                           static_cast<Float::ValueType>(b.value));
  return Int{static_cast<Int::ValueType>(result)};
}

inline Float floorDiv(Float a, Float b) {
  return Float{std::floor(a.value / b.value)};
}

// **
Value pow(const Value &a, const Value &b);

Float pow(Float a, Float b);
Float pow(Float a, Int b);

// ..
Value dotDot(const Value &a, const Value &b);
List dotDot(Int a, Int b);

// :
Value index(const Value &a, const Value &b);
Value &mutableIndex(const Value &a, const Value &b);

template <typename VT>
const auto &index(const List &a, const VT &i) {
  return a.at(i);
}
template <typename VT>
const auto &index(const Map &a, const VT &i) {
  return a.at(i);
}
inline Char index(const String &a, Int i) { return a.at(i); }

template <typename VT>
auto &mutableIndex(List &a, const VT &i) {
  return a[i];
}
template <typename VT>
auto &mutableIndex(Map &a, const VT &i) {
  return a[i];
}
inline Char &mutableIndex(String &a, Int i) { return a[i]; }

// !
Reference ref(Value &v);

// &
Value bitAnd(const Value &a, const Value &b);

inline Int bitAnd(Int a, Int b) { return Int{a.value & b.value}; }

// |
Value bitOr(const Value &a, const Value &b);

inline Int bitOr(Int a, Int b) { return Int{a.value | b.value}; }

// ^
Value bitXor(const Value &a, const Value &b);

inline Int bitXor(Int a, Int b) { return Int{a.value ^ b.value}; }

// ~
Value bitNot(const Value &a);
inline Int bitNot(Int a) { return Int{~a.value}; }

// <<
Value bitLShift(const Value &a, const Value &b);

inline Int bitLShift(Int a, Int b) { return Int{a.value << b.value}; }

// >>
Value bitRShift(const Value &a, const Value &b);

inline Int bitRShift(Int a, Int b) { return Int{a.value >> b.value}; }

// is
Bool is(const Value &a, std::string_view type);

// and
Bool logicalAnd(const Value &a, const Value &b);
inline Bool logicalAnd(Bool a, Bool b) { return Bool{a.value && b.value}; }
// or
Bool logicalOr(const Value &a, const Value &b);
inline Bool logicalOr(Bool a, Bool b) { return Bool{a.value || b.value}; }
// not
Bool logicalNot(const Value &a);
inline Bool logicalNot(Bool a) { return Bool{!a.value}; }

// <
Bool lessThan(const Value &a, const Value &b);
inline Bool lessThan(Int a, Int b) { return Bool{a.value < b.value}; }
inline Bool lessThan(Float a, Float b) { return Bool{a.value < b.value}; }
// <=
Bool lessEquals(const Value &a, const Value &b);
inline Bool lessEquals(Int a, Int b) { return Bool{a.value <= b.value}; }
inline Bool lessEquals(Float a, Float b) { return Bool{a.value <= b.value}; }
// >
Bool greaterThan(const Value &a, const Value &b);
inline Bool greaterThan(Int a, Int b) { return Bool{a.value > b.value}; }
inline Bool greaterThan(Float a, Float b) { return Bool{a.value > b.value}; }
// >=
Bool greaterEquals(const Value &a, const Value &b);
inline Bool greaterEquals(Int a, Int b) { return Bool{a.value >= b.value}; }
inline Bool greaterEquals(Float a, Float b) { return Bool{a.value >= b.value}; }

// ==
inline Bool equal(const Value &, const Value &);

inline Bool equal(None, None) { return Bool::t; }
inline Bool equal(Bool a, Bool b) { return Bool{a.value == b.value}; }
inline Bool equal(Int a, Int b) { return Bool{a.value == b.value}; }
inline Bool equal(Float a, Float b) { return Bool{a.value == b.value}; }
inline Bool equal(Char a, Char b) { return Bool{a.value == b.value}; }
inline Bool equal(const String &a, const String &b) {
  return Bool{a.value == b.value};
}
Bool equal(const Map &a, const Map &b);
Bool equal(const List &a, const List &b);
inline Bool equal(const Scope &, const Scope &) { return Bool::f; }
inline Bool equal(const Label &, const Label &) { return Bool::f; }
Bool equal(const UserObject &a, const UserObject &b);

template <typename... ValueTypes>
Bool equal(const BasicFlatValue<ValueTypes...> &,
           const BasicFlatValue<ValueTypes...> &);

// !=
template <typename VT1, typename VT2>
inline Bool notEqual(const VT1 &a, const VT2 &b) {
  return Bool{!equal(a, b).value};
}

// Map overload operations.
// The following operations cannot be overloaded:
//   ':', '::', ';', ';;', '!', 'unary /'
Value add(Map &a, const Value &b);
Value unaryPlus(Map &a);

Value sub(Map &a, const Value &b);
Value unaryMinus(Map &a);

Value mul(Map &a, const Value &b);
Value div(Map &a, const Value &b);
Value mod(Map &a, const Value &b);
Value floorDiv(Map &a, const Value &b);
Value pow(Map &a, const Value &b);

Value dotDot(Map &a, const Value &b);

Value bitAnd(Map &a, const Value &b);
Value bitOr(Map &a, const Value &b);
Value bitXor(Map &a, const Value &b);
Value bitNot(Map &a);

Value bitLShift(Map &a, const Value &b);
Value bitRShift(Map &a, const Value &b);

Bool logicalAnd(Map &a, const Value &b);
Bool logicalOr(Map &a, const Value &b);
Bool logicalNot(Map &a);

Bool lessThan(Map &a, const Value &b);
Bool lessEquals(Map &a, const Value &b);
Bool greaterThan(Map &a, const Value &b);
Bool greaterEquals(Map &a, const Value &b);

Bool equal(Map &a, const Value &b);
Bool notEqual(Map &a, const Value &b);

// UserObject overload operations
// The following operations cannot be overloaded:
//   '::', ';', ';;', '!', 'unary /'
Value index(const UserObject &a, const Value &b);
Value &mutableIndex(UserObject &a, const Value &b);

Value add(UserObject &a, const Value &b);
Value unaryPlus(UserObject &a);

Value sub(UserObject &a, const Value &b);
Value unaryMinus(UserObject &a);

Value mul(UserObject &a, const Value &b);
Value div(UserObject &a, const Value &b);
Value mod(UserObject &a, const Value &b);
Value floorDiv(UserObject &a, const Value &b);
Value pow(UserObject &a, const Value &b);

Value dotDot(UserObject &a, const Value &b);

Value bitAnd(UserObject &a, const Value &b);
Value bitOr(UserObject &a, const Value &b);
Value bitXor(UserObject &a, const Value &b);
Value bitNot(UserObject &a);

Value bitLShift(UserObject &a, const Value &b);
Value bitRShift(UserObject &a, const Value &b);

Bool logicalAnd(UserObject &a, const Value &b);
Bool logicalOr(UserObject &a, const Value &b);
Bool logicalNot(UserObject &a);

Bool lessThan(UserObject &a, const Value &b);
Bool lessEquals(UserObject &a, const Value &b);
Bool greaterThan(UserObject &a, const Value &b);
Bool greaterEquals(UserObject &a, const Value &b);

Bool equal(UserObject &a, const Value &b);
Bool notEqual(UserObject &a, const Value &b);
} // namespace ops

template <typename VT, detail::enableValidOpArgs<VT> * = nullptr>
auto operator++(VT &v) {
  return extense::ops::inc(v);
}

template <typename VT, detail::enableValidOpArgs<VT> * = nullptr>
auto operator--(VT &v) {
  return extense::ops::dec(v);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator+(const VT1 &a, const VT2 &b) {
  return extense::ops::add(a, b);
}

template <typename VT, detail::enableValidOpArgs<VT> * = nullptr>
auto operator+(const VT &v) {
  return extense::ops::unaryPlus(v);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator-(const VT1 &a, const VT2 &b) {
  return extense::ops::sub(a, b);
}

template <typename VT, detail::enableValidOpArgs<VT> * = nullptr>
auto operator-(const VT &v) {
  return extense::ops::unaryMinus(v);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator*(const VT1 &a, const VT2 &b) {
  return extense::ops::mul(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator/(const VT1 &a, const VT2 &b) {
  return extense::ops::div(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator%(const VT1 &a, const VT2 &b) {
  return extense::ops::mod(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator&(const VT1 &a, const VT2 &b) {
  return extense::ops::bitAnd(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator|(const VT1 &a, const VT2 &b) {
  return extense::ops::bitOr(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator^(const VT1 &a, const VT2 &b) {
  return extense::ops::bitXor(a, b);
}

template <typename VT, detail::enableValidOpArgs<VT> * = nullptr>
auto operator~(const VT &a) {
  return extense::ops::bitNot(a);
}

inline auto operator<<(extense::Int a, extense::Int b) {
  return extense::ops::bitLShift(a, b);
}

inline auto operator>>(extense::Int a, extense::Int b) {
  return extense::ops::bitRShift(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator&&(const VT1 &a, const VT2 &b) {
  return extense::ops::logicalAnd(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator||(const VT1 &a, const VT2 &b) {
  return extense::ops::logicalOr(a, b);
}

template <typename VT, detail::enableValidOpArgs<VT> * = nullptr>
auto operator!(const VT &a) {
  return extense::ops::logicalNot(a);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator<(const VT1 &a, const VT2 &b) {
  return extense::ops::lessThan(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator<=(const VT1 &a, const VT2 &b) {
  return extense::ops::lessEquals(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator>(const VT1 &a, const VT2 &b) {
  return extense::ops::greaterThan(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator>=(const VT1 &a, const VT2 &b) {
  return extense::ops::greaterEquals(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator==(const VT1 &a, const VT2 &b) {
  return extense::ops::equal(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator!=(const VT1 &a, const VT2 &b) {
  return extense::ops::notEqual(a, b);
}
} // namespace extense

#endif /* _LIB_EXTENSE_DETAIL__OPERATIONS_HPP */
