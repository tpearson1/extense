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

// + +=
Value add(const Value &a, const Value &b);
Value addEquals(Value &a, const Value &b);

inline Int add(Int a, Int b) { return Int{a.value + b.value}; }
inline Int &addEquals(Int &a, Int b) {
  a.value += b.value;
  return a;
}

inline Float add(Float a, Float b) { return Float{a.value + b.value}; }
inline Float &addEquals(Float &a, Float b) {
  a.value += b.value;
  return a;
}

inline String add(const String &a, const String &b) {
  return String{a.value + b.value};
}
inline String &addEquals(String &a, const String &b) {
  a.value += b.value;
  return a;
}

List add(List a, const List &b);
List &addEquals(List &a, const List &b);

// +x
Value add(const Value &a);
inline Int add(Int a) { return a; }
inline Float add(Float a) { return a; }

// - -=
Value sub(const Value &a, const Value &b);
Value subEquals(Value &a, const Value &b);

inline Int sub(Int a, Int b) { return Int{a.value - b.value}; }
inline Int &subEquals(Int &a, Int b) {
  a.value -= b.value;
  return a;
}

inline Float sub(Float a, Float b) { return Float{a.value - b.value}; }
inline Float &subEquals(Float &a, Float b) {
  a.value -= b.value;
  return a;
}

// Unary negation (-x)
Value sub(const Value &a);
inline Int sub(Int a) { return Int{-a.value}; }
inline Float sub(Float a) { return Float{-a.value}; }

// * *=
Value mul(const Value &a, const Value &b);
Value mulEquals(Value &a, const Value &b);

inline Int mul(Int a, Int b) { return Int{a.value * b.value}; }
inline Int &mulEquals(Int &a, Int b) {
  a.value *= b.value;
  return a;
}

inline Float mul(Float a, Float b) { return Float{a.value * b.value}; }
inline Float &mulEquals(Float &a, Float b) {
  a.value *= b.value;
  return a;
}

String mul(String a, Int times);
String &mulEquals(String &a, Int times);

List mul(List a, Int times);
List &mulEquals(List &a, Int times);

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

// / /=
Value div(const Value &a, const Value &b);
Value divEquals(Value &a, const Value &b);

inline Float div(Float a, Float b) {
  checkDivision(b);
  return Float{a.value / b.value};
}
inline Float &divEquals(Float &a, Float b) {
  a = div(a, b);
  return a;
}

// % %=
Value mod(const Value &a, const Value &b);
Value modEquals(Value &a, const Value &b);

inline Int mod(Int a, Int b) {
  checkDivision(b);
  return Int{a.value % b.value};
}
inline Int &modEquals(Int &a, Int b) {
  a = mod(a, b);
  return a;
}

inline Float mod(Float a, Float b) {
  checkDivision(b);
  return Float{std::fmod(a.value, b.value)};
}
inline Float &modEquals(Float &a, Float b) {
  a = mod(a, b);
  return a;
}

// // //=
Value floorDiv(const Value &a, const Value &b);
Value floorDivEquals(Value &a, const Value &b);

inline Int floorDiv(Int a, Int b) {
  checkDivision(b);
  auto result = std::floor(static_cast<Float::ValueType>(a.value) /
                           static_cast<Float::ValueType>(b.value));
  return Int{static_cast<Int::ValueType>(result)};
}
inline Int &floorDivEquals(Int &a, Int b) {
  a = floorDiv(a, b);
  return a;
}

inline Float floorDiv(Float a, Float b) {
  return Float{std::floor(a.value / b.value)};
}
inline Float &floorDivEquals(Float &a, Float b) {
  a = floorDiv(a, b);
  return a;
}

// ** **=
Value pow(const Value &a, const Value &b);
Value powEquals(Value &a, const Value &b);

Float pow(Float a, Float b);
inline Float &powEquals(Float &a, Float b) {
  a = pow(a, b);
  return a;
}

Float pow(Float a, Int b);
inline Float &powEquals(Float &a, Int b) {
  a = pow(a, b);
  return a;
}

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

// & &=
Value bitAnd(const Value &a, const Value &b);
Value bitAndEquals(Value &a, const Value &b);

inline Int bitAnd(Int a, Int b) { return Int{a.value & b.value}; }
inline Int &bitAndEquals(Int &a, Int b) {
  a = bitAnd(a, b);
  return a;
}

// | |=
Value bitOr(const Value &a, const Value &b);
Value bitOrEquals(Value &a, const Value &b);

inline Int bitOr(Int a, Int b) { return Int{a.value | b.value}; }
inline Int &bitOrEquals(Int &a, Int b) {
  a = bitOr(a, b);
  return a;
}

// ^ ^=
Value bitXor(const Value &a, const Value &b);
Value bitXorEquals(Value &a, const Value &b);

inline Int bitXor(Int a, Int b) { return Int{a.value ^ b.value}; }
inline Int &bitXorEquals(Int &a, Int b) {
  a = bitXor(a, b);
  return a;
}

// ~
Value bitNot(const Value &a);
inline Int bitNot(Int a) { return Int{~a.value}; }

// << <<=
Value bitLShift(const Value &a, const Value &b);
Value bitLShiftEquals(Value &a, const Value &b);

inline Int bitLShift(Int a, Int b) { return Int{a.value << b.value}; }
inline Int &bitLShiftEquals(Int &a, Int b) {
  a = bitLShift(a, b);
  return a;
}

// >> >>=
Value bitRShift(const Value &a, const Value &b);
Value bitRShiftEquals(Value &a, const Value &b);

inline Int bitRShift(Int a, Int b) { return Int{a.value >> b.value}; }
inline Int &bitRShiftEquals(Int &a, Int b) {
  a = bitRShift(a, b);
  return a;
}

// is
Bool is(const Value &a, std::string_view type);

// and
Value logicalAnd(const Value &a, const Value &b);
inline Bool logicalAnd(Bool a, Bool b) { return Bool{a.value && b.value}; }
// or
Value logicalOr(const Value &a, const Value &b);
inline Bool logicalOr(Bool a, Bool b) { return Bool{a.value || b.value}; }
// not
Value logicalNot(const Value &a);
inline Bool logicalNot(Bool a) { return Bool{!a.value}; }

// <
Value lessThan(const Value &a, const Value &b);
inline Bool lessThan(Int a, Int b) { return Bool{a.value < b.value}; }
inline Bool lessThan(Float a, Float b) { return Bool{a.value < b.value}; }
// <=
Value lessEquals(const Value &a, const Value &b);
inline Bool lessEquals(Int a, Int b) { return Bool{a.value <= b.value}; }
inline Bool lessEquals(Float a, Float b) { return Bool{a.value <= b.value}; }
// >
Value greaterThan(const Value &a, const Value &b);
inline Bool greaterThan(Int a, Int b) { return Bool{a.value > b.value}; }
inline Bool greaterThan(Float a, Float b) { return Bool{a.value > b.value}; }
// >=
Value greaterEquals(const Value &a, const Value &b);
inline Bool greaterEquals(Int a, Int b) { return Bool{a.value >= b.value}; }
inline Bool greaterEquals(Float a, Float b) { return Bool{a.value >= b.value}; }

// ==
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
inline Bool equal(const UserObject &, const UserObject &) { return Bool::f; }

template <typename... ValueTypes>
Bool equal(const BasicFlatValue<ValueTypes...> &,
           const BasicFlatValue<ValueTypes...> &);

inline Bool equal(const Value &, const Value &);

// !=
template <typename VT1, typename VT2>
inline Bool notEqual(const VT1 &a, const VT2 &b) {
  return Bool{!equal(a, b).value};
}

// Map overload operations.
// The following operations cannot be overloaded:
//   ':', '::', ';', ';;', '!', 'unary /'
Value add(Map &a, const Value &b);
Value addEquals(Map &a, const Value &b);
Value add(Map &a);

Value sub(Map &a, const Value &b);
Value subEquals(Map &a, const Value &b);
Value sub(Map &a);

Value mul(Map &a, const Value &b);
Value mulEquals(Map &a, const Value &b);

Value div(Map &a, const Value &b);
Value divEquals(Map &a, const Value &b);

Value mod(Map &a, const Value &b);
Value modEquals(Map &a, const Value &b);

Value floorDiv(Map &a, const Value &b);
Value floorDivEquals(Map &a, const Value &b);

Value pow(Map &a, const Value &b);
Value powEquals(Map &a, const Value &b);

Value dotDot(Map &a, const Value &b);

Value bitAnd(Map &a, const Value &b);
Value bitAndEquals(Map &a, const Value &b);

Value bitOr(Map &a, const Value &b);
Value bitOrEquals(Map &a, const Value &b);

Value bitXor(Map &a, const Value &b);
Value bitXorEquals(Map &a, const Value &b);

Value bitNot(Map &a);

Value bitLShift(Map &a, const Value &b);
Value bitLShiftEquals(Map &a, const Value &b);

Value bitRShift(Map &a, const Value &b);
Value bitRShiftEquals(Map &a, const Value &b);

Value logicalAnd(Map &a, const Value &b);
Value logicalOr(Map &a, const Value &b);

Value lessThan(Map &a, const Value &b);
Value lessEquals(Map &a, const Value &b);
Value greaterThan(Map &a, const Value &b);
Value greaterEquals(Map &a, const Value &b);

Value equal(Map &a, const Value &b);
Value notEqual(Map &a, const Value &b);

// UserObject overload operations
// The following operations cannot be overloaded:
//   '::', ';', ';;', '!', 'unary /'
Value index(const UserObject &a, const Value &b);
Value &mutableIndex(UserObject &a, const Value &b);

Value add(UserObject &a, const Value &b);
Value addEquals(UserObject &a, const Value &b);
Value add(UserObject &a);

Value sub(UserObject &a, const Value &b);
Value subEquals(UserObject &a, const Value &b);
Value sub(UserObject &a);

Value mul(UserObject &a, const Value &b);
Value mulEquals(UserObject &a, const Value &b);

Value div(UserObject &a, const Value &b);
Value divEquals(UserObject &a, const Value &b);

Value mod(UserObject &a, const Value &b);
Value modEquals(UserObject &a, const Value &b);

Value floorDiv(UserObject &a, const Value &b);
Value floorDivEquals(UserObject &a, const Value &b);

Value pow(UserObject &a, const Value &b);
Value powEquals(UserObject &a, const Value &b);

Value dotDot(UserObject &a, const Value &b);

Value bitAnd(UserObject &a, const Value &b);
Value bitAndEquals(UserObject &a, const Value &b);

Value bitOr(UserObject &a, const Value &b);
Value bitOrEquals(UserObject &a, const Value &b);

Value bitXor(UserObject &a, const Value &b);
Value bitXorEquals(UserObject &a, const Value &b);

Value bitNot(UserObject &a);

Value bitLShift(UserObject &a, const Value &b);
Value bitLShiftEquals(UserObject &a, const Value &b);

Value bitRShift(UserObject &a, const Value &b);
Value bitRShiftEquals(UserObject &a, const Value &b);

Value logicalAnd(UserObject &a, const Value &b);
Value logicalOr(UserObject &a, const Value &b);

Value lessThan(UserObject &a, const Value &b);
Value lessEquals(UserObject &a, const Value &b);
Value greaterThan(UserObject &a, const Value &b);
Value greaterEquals(UserObject &a, const Value &b);

Value equal(UserObject &a, const Value &b);
Value notEqual(UserObject &a, const Value &b);
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
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator+=(VT1 &a, const VT2 &b) {
  return extense::ops::addEquals(a, b);
}

template <typename VT, detail::enableValidOpArgs<VT> * = nullptr>
auto operator+(const VT &v) {
  return extense::ops::add(v);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator-(const VT1 &a, const VT2 &b) {
  return extense::ops::sub(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator-=(VT1 &a, const VT2 &b) {
  return extense::ops::subEquals(a, b);
}

template <typename VT, detail::enableValidOpArgs<VT> * = nullptr>
auto operator-(const VT &v) {
  return extense::ops::sub(v);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator*(const VT1 &a, const VT2 &b) {
  return extense::ops::mul(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator*=(VT1 &a, const VT2 &b) {
  return extense::ops::mulEquals(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator/(const VT1 &a, const VT2 &b) {
  return extense::ops::div(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator/=(VT1 &a, const VT2 &b) {
  return extense::ops::divEquals(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator%(const VT1 &a, const VT2 &b) {
  return extense::ops::mod(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator%=(VT1 &a, const VT2 &b) {
  return extense::ops::modEquals(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator&(const VT1 &a, const VT2 &b) {
  return extense::ops::bitAnd(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator&=(VT1 &a, const VT2 &b) {
  return extense::ops::bitAndEquals(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator|(const VT1 &a, const VT2 &b) {
  return extense::ops::bitOr(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator|=(VT1 &a, const VT2 &b) {
  return extense::ops::bitOrEquals(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator^(const VT1 &a, const VT2 &b) {
  return extense::ops::bitXor(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator^=(VT1 &a, const VT2 &b) {
  return extense::ops::bitXorEquals(a, b);
}

template <typename VT, detail::enableValidOpArgs<VT> * = nullptr>
auto operator~(const VT &a) {
  return extense::ops::bitNot(a);
}

inline auto operator<<(extense::Int a, extense::Int b) {
  return extense::ops::bitLShift(a, b);
}
inline auto operator<<=(extense::Int &a, extense::Int b) {
  return extense::ops::bitLShiftEquals(a, b);
}

inline auto operator>>(extense::Int a, extense::Int b) {
  return extense::ops::bitRShift(a, b);
}
inline auto operator>>=(extense::Int &a, extense::Int b) {
  return extense::ops::bitRShiftEquals(a, b);
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
