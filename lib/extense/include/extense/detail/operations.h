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

#ifndef _LIB_EXTENSE_DETAIL__OPERATIONS_H
#define _LIB_EXTENSE_DETAIL__OPERATIONS_H

#include <cmath>

#include <extense/detail/types.h>

namespace extense {
// TODO: Exception hierarchy for execution, with base Exception class
/*
 * Exception thrown when an operation is found to be invalid, such as the
 * addition of two Values with contained type Char.
 */
class InvalidOperation : public std::exception {
  std::string v1, v2, reason;
  std::string whatStr;

  inline static constexpr auto defaultMessage = "Incompatible types";

  void buildWhatStr() {
    whatStr =
        "Invalid operation with types '" + v1 + "' + '" + v2 + "': " + reason;
  }

public:
  InvalidOperation(std::string firstType, std::string secondType,
                   std::string message = defaultMessage)
      : v1(std::move(firstType)), v2(std::move(secondType)),
        reason(std::move(message)) {
    buildWhatStr();
  }

  template <typename V1, typename V2>
  static InvalidOperation Create(std::string message = defaultMessage) {
    return InvalidOperation{std::string{typeAsString<V1>},
                            std::string{typeAsString<V2>}, std::move(message)};
  }

  const std::string &firstType() const noexcept { return v1; }
  const std::string &secondType() const noexcept { return v2; }
  const std::string &message() const noexcept { return reason; }

  virtual const char *what() const noexcept override { return whatStr.c_str(); }
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
// TODO: Implement operators for Set once functions are implemented as Values

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
inline Int add(Int a) { return a; }
inline Float add(Float a) { return a; }

// - -=
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
inline Int sub(Int a) { return Int{-a.value}; }
inline Float sub(Float a) { return Float{-a.value}; }

// * *=
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

namespace {
template <typename ValueType>
inline static void checkDivision(ValueType b) {
  if (b.value != 0) return;
  throw InvalidOperation::Create<ValueType, ValueType>(
      "Division/modulo by zero");
}
} // namespace

// / /=
inline Float div(Float a, Float b) {
  checkDivision(b);
  return Float{a.value / b.value};
}
inline Float &divEquals(Float &a, Float b) {
  a = div(a, b);
  return a;
}

// % %=
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
List dotDot(Int a, Int b);

// :
template <typename VT1, typename VT2>
auto index(const VT1 &a, const VT2 &i) {
  return a[i];
}

// Reflexive indexing (::) can only be implemented once functions are added as a
// type

// !
Reference ref(Value &v);

// & &=
inline Int bitAnd(Int a, Int b) { return Int{a.value & b.value}; }
inline Int &bitAndEquals(Int &a, Int b) {
  a = bitAnd(a, b);
  return a;
}

// | |=
inline Int bitOr(Int a, Int b) { return Int{a.value | b.value}; }
inline Int &bitOrEquals(Int &a, Int b) {
  a = bitOr(a, b);
  return a;
}

// ^ ^=
inline Int bitXor(Int a, Int b) { return Int{a.value ^ b.value}; }
inline Int &bitXorEquals(Int &a, Int b) {
  a = bitXor(a, b);
  return a;
}

// ~
inline Int bitNot(Int a) { return Int{~a.value}; }

// << <<=
inline Int bitLShift(Int a, Int b) { return Int{a.value << b.value}; }
inline Int &bitLShiftEquals(Int &a, Int b) {
  a = bitLShift(a, b);
  return a;
}

// >> >>=
inline Int bitRShift(Int a, Int b) { return Int{a.value >> b.value}; }
inline Int &bitRShiftEquals(Int &a, Int b) {
  a = bitRShift(a, b);
  return a;
}

// and
inline Bool logicalAnd(Bool a, Bool b) { return Bool{a.value && b.value}; }
// or
inline Bool logicalOr(Bool a, Bool b) { return Bool{a.value || b.value}; }
// not
inline Bool logicalNot(Bool a) { return Bool{!a.value}; }

// <
inline Bool lessThan(Int a, Int b) { return Bool{a.value < b.value}; }
inline Bool lessThan(Float a, Float b) { return Bool{a.value < b.value}; }
// <=
inline Bool lessEquals(Int a, Int b) { return Bool{a.value <= b.value}; }
inline Bool lessEquals(Float a, Float b) { return Bool{a.value <= b.value}; }
// >
inline Bool greaterThan(Int a, Int b) { return Bool{a.value > b.value}; }
inline Bool greaterThan(Float a, Float b) { return Bool{a.value > b.value}; }
// >=
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
Bool equal(const Set &a, const Set &b);
Bool equal(const List &a, const List &b);

template <typename... ValueTypes>
Bool equal(const BasicFlatValue<ValueTypes...> &,
           const BasicFlatValue<ValueTypes...> &);

inline Bool equal(const Value &, const Value &);

// !=
template <typename VT1, typename VT2>
inline Bool notEqual(const VT1 &a, const VT2 &b) {
  return Bool{!equal(a, b).value};
}

// is - not implemented as an operation here because it doesn't depend on any
// arguments
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
VT1 &operator+=(VT1 &a, const VT2 &b) {
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
VT1 &operator-=(VT1 &a, const VT2 &b) {
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
VT1 &operator*=(VT1 &a, const VT2 &b) {
  return extense::ops::mulEquals(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator/(const VT1 &a, const VT2 &b) {
  return extense::ops::div(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
VT1 &operator/=(VT1 &a, const VT2 &b) {
  return extense::ops::divEquals(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator%(const VT1 &a, const VT2 &b) {
  return extense::ops::mod(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
VT1 &operator%=(VT1 &a, const VT2 &b) {
  return extense::ops::modEquals(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator&(const VT1 &a, const VT2 &b) {
  return extense::ops::bitAnd(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
VT1 &operator&=(VT1 &a, const VT2 &b) {
  return extense::ops::bitAndEquals(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator|(const VT1 &a, const VT2 &b) {
  return extense::ops::bitOr(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
VT1 &operator|=(VT1 &a, const VT2 &b) {
  return extense::ops::bitOrEquals(a, b);
}

template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
auto operator^(const VT1 &a, const VT2 &b) {
  return extense::ops::bitXor(a, b);
}
template <typename VT1, typename VT2,
          detail::enableValidOpArgs<VT1, VT2> * = nullptr>
VT1 &operator^=(VT1 &a, const VT2 &b) {
  return extense::ops::bitXorEquals(a, b);
}

template <typename VT, detail::enableValidOpArgs<VT> * = nullptr>
auto operator~(const VT &a) {
  return extense::ops::bitNot(a);
}

inline auto operator<<(extense::Int a, extense::Int b) {
  return extense::ops::bitLShift(a, b);
}
inline auto &operator<<=(extense::Int &a, extense::Int b) {
  return extense::ops::bitLShiftEquals(a, b);
}

inline auto operator>>(extense::Int a, extense::Int b) {
  return extense::ops::bitRShift(a, b);
}
inline auto &operator>>=(extense::Int &a, extense::Int b) {
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

#endif /* _LIB_EXTENSE_DETAIL__OPERATIONS_H */
