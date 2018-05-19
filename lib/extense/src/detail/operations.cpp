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

#include <extense/value.hpp>

#include <numeric>

using namespace extense::literals;

extense::InvalidBinaryOperation::InvalidBinaryOperation(const Value &a,
                                                        const Value &b,
                                                        std::string reason)
    : Exception("") {
  setError(build(a.typeAsString(), b.typeAsString(), std::move(reason)));
  setType("InvalidBinaryOperation");
}

extense::InvalidUnaryOperation::InvalidUnaryOperation(const Value &a,
                                                      std::string reason)
    : Exception("") {
  setError(build(a.typeAsString(), std::move(reason)));
  setType("InvalidUnaryOperation");
}

namespace extense::ops {
#define VALID_OP(resultVar, op)                                                \
  template <typename A, typename B, typename = void>                           \
  struct resultVar##Helper : std::false_type {};                               \
                                                                               \
  template <typename A, typename B>                                            \
  struct resultVar##Helper<                                                    \
      A, B,                                                                    \
      std::void_t<decltype(op(std::declval<A &>(), std::declval<B &>()))>>     \
      : std::true_type {};                                                     \
                                                                               \
  template <typename A, typename B>                                            \
  constexpr bool resultVar = resultVar##Helper<A, B>::value;

#define OP_VISITOR(opFuncName)                                                 \
  VALID_OP(canDoOperation##opFuncName, opFuncName)                             \
                                                                               \
  Value opFuncName(const Value &a, const Value &b) {                           \
    if (a.is<Map>()) return opFuncName(mutableGet<Map>(a), b);                 \
    if (a.is<UserObject>())                                                    \
      return Value{opFuncName(mutableGet<UserObject>(a), b)};                  \
                                                                               \
    return visit(                                                              \
        [](const auto &a, const auto &b) -> Value {                            \
          using A = std::decay_t<decltype(a)>;                                 \
          using B = std::decay_t<decltype(b)>;                                 \
          if constexpr (!canDoOperation##opFuncName<A, B>)                     \
            throw InvalidBinaryOperation::Create<A, B>();                      \
          else                                                                 \
            return Value{opFuncName(a, b)};                                    \
        },                                                                     \
        a, b);                                                                 \
  }

#define COMPOUND_OP_VISITOR(opFuncName, nonCompoundFuncName)                   \
  VALID_OP(canDoOperation##opFuncName, opFuncName)                             \
                                                                               \
  Value opFuncName(Value &av, const Value &bv) {                               \
    if (av.is<Map>()) return opFuncName(get<Map>(av), bv);                     \
    if (av.is<UserObject>()) return opFuncName(get<UserObject>(av), bv);       \
                                                                               \
    visit(                                                                     \
        [&av, &bv](auto &a, const auto &b) {                                   \
          using A = std::decay_t<decltype(a)>;                                 \
          using B = std::decay_t<decltype(b)>;                                 \
          if constexpr (!canDoOperation##opFuncName<A, B>) {                   \
            /* It may be the case that the operation is possible, but */       \
            /* the mutable argument needs to be converted */                   \
            av = nonCompoundFuncName(av, bv);                                  \
          } else                                                               \
            opFuncName(a, b);                                                  \
        },                                                                     \
        av, bv);                                                               \
    return noneValue;                                                          \
  }

Value unaryPlus(const Value &a) {
  if (a.is<Int>() || a.is<Float>()) return a;
  throw InvalidUnaryOperation{a};
}

OP_VISITOR(add)
COMPOUND_OP_VISITOR(addEquals, add)

List add(List a, const List &b) {
  addEquals(a, b);
  return a;
}

List &addEquals(List &a, const List &b) {
  auto &av = a.value;
  auto &bv = b.value;
  av.insert(std::end(av), std::begin(bv), std::end(bv));
  return a;
}

Value unaryMinus(const Value &a) {
  if (a.is<Int>()) return Value{-get<Int>(a)};
  if (a.is<Float>()) return Value{-get<Float>(a)};
  throw InvalidUnaryOperation{a};
}

OP_VISITOR(sub)
COMPOUND_OP_VISITOR(subEquals, sub)

OP_VISITOR(mul)
COMPOUND_OP_VISITOR(mulEquals, mul)

String mul(String a, Int times) {
  mulEquals(a, times);
  return a;
}

template <typename ValueType>
static ValueType &mulEquals(ValueType &a, Int times_) {
  if (times_.value < 0) {
    throw InvalidBinaryOperation::Create<ValueType, Int>(
        "Expected Int to be greater than 0");
  }
  auto times = static_cast<std::size_t>(times_.value);

  auto original = a;
  a.value.clear();
  a.value.reserve(original.value.size() * times);
  for (std::size_t i = 0; i < times; i++) addEquals(a, original);
  return a;
}

String &mulEquals(String &a, Int times) { return mulEquals<String>(a, times); }

List mul(List a, Int times) {
  mulEquals(a, times);
  return a;
}

List &mulEquals(List &a, Int times) { return mulEquals<List>(a, times); }

OP_VISITOR(div)
COMPOUND_OP_VISITOR(divEquals, div)

OP_VISITOR(mod)
COMPOUND_OP_VISITOR(modEquals, mod)

OP_VISITOR(floorDiv)
COMPOUND_OP_VISITOR(floorDivEquals, floorDiv)

OP_VISITOR(pow)
COMPOUND_OP_VISITOR(powEquals, pow)

constexpr const auto powMessage = "Invalid values for base and/or exponent";

// The power functions consider raising 0 to the 0 as 1
Float pow(Float a, Float b) {
  if (a.value < 0.0 || (a.value == 0.0 && b.value < 0.0))
    throw InvalidBinaryOperation::Create<Float, Float>(powMessage);
  return Float{std::pow(a.value, b.value)};
}

Float pow(Float a, Int b) {
  if (a.value == 0.0 && b.value < 0)
    throw InvalidBinaryOperation::Create<Float, Int>(powMessage);
  return Float{std::pow(a.value, b.value)};
}

OP_VISITOR(dotDot)

List dotDot(Int a, Int b) {
  if (b.value < a.value) {
    throw InvalidBinaryOperation::Create<Int, Int>(
        "Operator '..' requires upper "
        "bound to be greater than or "
        "equal to lower bound");
  }
  // Should contain b - a + 1 elements (a, a+1, a+2, ... b)
  List::ValueType list(b.value - a.value + 1);
  std::iota(std::begin(list), std::end(list), a);
  return List(list);
}

Value index(const Value &a, const Value &b) {
  if (a.is<Map>()) return get<Map>(a).at(b);
  if (a.is<List>()) return get<List>(a).at(b);
  if (a.is<UserObject>()) return get<UserObject>(a).at(b);

  if (!a.is<String>() || !b.is<Int>())
    throw InvalidBinaryOperation(a, b, "Unable to index type");
  return Value{get<String>(a).at(get<Int>(b))};
}

Value &mutableIndex(const Value &a, const Value &b) {
  if (a.is<Map>()) return mutableGet<Map>(a)[b];
  if (a.is<UserObject>()) return mutableGet<UserObject>(a)[b];

  if (!a.is<List>()) throw InvalidBinaryOperation(a, b, "Unable to index type");
  if (!b.is<Int>())
    throw InvalidBinaryOperation("List", b.typeAsString(),
                                 "Unable to index type");

  if (b.is<List>())
    throw InvalidBinaryOperation("List", "List", "Unable to mutate sub-list");
  return mutableGet<List>(a)[get<Int>(b)];
}

Reference ref(Value &v) { return Reference{v}; }

OP_VISITOR(bitAnd)
COMPOUND_OP_VISITOR(bitAndEquals, bitAnd)

OP_VISITOR(bitOr)
COMPOUND_OP_VISITOR(bitOrEquals, bitOr)

OP_VISITOR(bitXor)
COMPOUND_OP_VISITOR(bitXorEquals, bitXor)

Value bitNot(const Value &a) {
  if (a.is<Int>()) return Value{bitNot(get<Int>(a))};
  throw InvalidUnaryOperation{a};
}

OP_VISITOR(bitLShift)
COMPOUND_OP_VISITOR(bitLShiftEquals, bitLShift)

OP_VISITOR(bitRShift)
COMPOUND_OP_VISITOR(bitRShiftEquals, bitRShift)

Bool is(const Value &a, std::string_view type) {
  if (a.is<Reference>() && type == "Reference") return Bool::t;
  return Bool{a.typeAsString(false) == type};
}

OP_VISITOR(logicalAnd)
OP_VISITOR(logicalOr)

Value logicalNot(const Value &a) {
  if (a.is<Bool>()) return Value{logicalNot(get<Bool>(a))};
  throw InvalidUnaryOperation{a};
}

OP_VISITOR(lessThan)
OP_VISITOR(lessEquals)
OP_VISITOR(greaterThan)
OP_VISITOR(greaterEquals)

Bool equal(const Map &a, const Map &b) { return Bool{a.value == b.value}; }
Bool equal(const List &a, const List &b) { return Bool{a.value == b.value}; }
Bool equal(const UserObject &a, const UserObject &b) {
  return a.equal(Value{b});
}
Bool equal(const Proxy &a, const Proxy &b) { return Bool{a.get() == b.get()}; }
} // namespace extense::ops

static extense::Value binaryFunction(const extense::String &op, extense::Map &a,
                                     const extense::Value &b) {
  try {
    auto v = extense::Value{a};
    auto ret = extense::get<extense::Scope>(a[op])(v, b);
    a = extense::get<extense::Map>(v);
    return ret;
  } catch (const extense::ValueGetError &) {
    throw extense::OperatorOverloadError{};
  }
}
static extense::Value unaryFunction(const extense::String &op,
                                    extense::Map &a) {
  try {
    auto v = extense::Value{a};
    auto ret = extense::get<extense::Scope>(a[op])(v);
    a = extense::get<extense::Map>(v);
    return ret;
  } catch (const extense::ValueGetError &) {
    throw extense::OperatorOverloadError{};
  }
}

namespace extense::ops {
// Map operations
Value add(Map &a, const Value &b) { return binaryFunction("+"_es, a, b); }
Value addEquals(Map &a, const Value &b) {
  return binaryFunction("+="_es, a, b);
}
Value unaryPlus(Map &a) { return unaryFunction("+"_es, a); }

Value sub(Map &a, const Value &b) { return binaryFunction("-"_es, a, b); }
Value subEquals(Map &a, const Value &b) {
  return binaryFunction("-="_es, a, b);
}
Value unaryMinus(Map &a) { return unaryFunction("-"_es, a); }

Value mul(Map &a, const Value &b) { return binaryFunction("*"_es, a, b); }
Value mulEquals(Map &a, const Value &b) {
  return binaryFunction("*="_es, a, b);
}

Value div(Map &a, const Value &b) { return binaryFunction("/"_es, a, b); }
Value divEquals(Map &a, const Value &b) {
  return binaryFunction("/="_es, a, b);
}

Value mod(Map &a, const Value &b) { return binaryFunction("%"_es, a, b); }
Value modEquals(Map &a, const Value &b) {
  return binaryFunction("%="_es, a, b);
}

Value floorDiv(Map &a, const Value &b) { return binaryFunction("//"_es, a, b); }
Value floorDivEquals(Map &a, const Value &b) {
  return binaryFunction("//="_es, a, b);
}

Value pow(Map &a, const Value &b) { return binaryFunction("**"_es, a, b); }
Value powEquals(Map &a, const Value &b) {
  return binaryFunction("**="_es, a, b);
}

Value dotDot(Map &a, const Value &b) { return binaryFunction(".."_es, a, b); }

Value bitAnd(Map &a, const Value &b) { return binaryFunction("&"_es, a, b); }
Value bitAndEquals(Map &a, const Value &b) {
  return binaryFunction("&="_es, a, b);
}

Value bitOr(Map &a, const Value &b) { return binaryFunction("|"_es, a, b); }
Value bitOrEquals(Map &a, const Value &b) {
  return binaryFunction("|="_es, a, b);
}

Value bitXor(Map &a, const Value &b) { return binaryFunction("^"_es, a, b); }
Value bitXorEquals(Map &a, const Value &b) {
  return binaryFunction("^="_es, a, b);
}

Value bitNot(Map &a) { return unaryFunction("~"_es, a); }

Value bitLShift(Map &a, const Value &b) {
  return binaryFunction("<<"_es, a, b);
}
Value bitLShiftEquals(Map &a, const Value &b) {
  return binaryFunction("<<="_es, a, b);
}

Value bitRShift(Map &a, const Value &b) {
  return binaryFunction(">>"_es, a, b);
}
Value bitRShiftEquals(Map &a, const Value &b) {
  return binaryFunction(">>="_es, a, b);
}

Value logicalAnd(Map &a, const Value &b) {
  return binaryFunction("and"_es, a, b);
}
Value logicalOr(Map &a, const Value &b) {
  return binaryFunction("or"_es, a, b);
}

Value lessThan(Map &a, const Value &b) { return binaryFunction("<"_es, a, b); }
Value lessEquals(Map &a, const Value &b) {
  return binaryFunction("<="_es, a, b);
}
Value greaterThan(Map &a, const Value &b) {
  return binaryFunction(">"_es, a, b);
}
Value greaterEquals(Map &a, const Value &b) {
  return binaryFunction(">="_es, a, b);
}

// UserObject operations
Value index(const UserObject &a, const Value &b) { return a.at(b); }
Value &mutableIndex(UserObject &a, const Value &b) { return a[b]; }

Value add(UserObject &a, const Value &b) { return a.add(b); }
Value addEquals(UserObject &a, const Value &b) { return a.addEquals(b); }
Value unaryPlus(UserObject &a) { return a.unaryPlus(); }

Value sub(UserObject &a, const Value &b) { return a.sub(b); }
Value subEquals(UserObject &a, const Value &b) { return a.subEquals(b); }
Value unaryMinus(UserObject &a) { return a.unaryMinus(); }

Value mul(UserObject &a, const Value &b) { return a.mul(b); }
Value mulEquals(UserObject &a, const Value &b) { return a.mulEquals(b); }

Value div(UserObject &a, const Value &b) { return a.div(b); }
Value divEquals(UserObject &a, const Value &b) { return a.divEquals(b); }

Value mod(UserObject &a, const Value &b) { return a.mod(b); }
Value modEquals(UserObject &a, const Value &b) { return a.modEquals(b); }

Value floorDiv(UserObject &a, const Value &b) { return a.floorDiv(b); }
Value floorDivEquals(UserObject &a, const Value &b) {
  return a.floorDivEquals(b);
}

Value pow(UserObject &a, const Value &b) { return a.pow(b); }
Value powEquals(UserObject &a, const Value &b) { return a.powEquals(b); }

Value dotDot(UserObject &a, const Value &b) { return a.dotDot(b); }

Value bitAnd(UserObject &a, const Value &b) { return a.bitAnd(b); }
Value bitAndEquals(UserObject &a, const Value &b) { return a.bitAndEquals(b); }

Value bitOr(UserObject &a, const Value &b) { return a.bitOr(b); }
Value bitOrEquals(UserObject &a, const Value &b) { return a.bitOrEquals(b); }

Value bitXor(UserObject &a, const Value &b) { return a.bitXor(b); }
Value bitXorEquals(UserObject &a, const Value &b) { return a.bitXorEquals(b); }

Value bitNot(UserObject &a) { return a.bitNot(); }

Value bitLShift(UserObject &a, const Value &b) { return a.bitLShift(b); }
Value bitLShiftEquals(UserObject &a, const Value &b) {
  return a.bitLShiftEquals(b);
}

Value bitRShift(UserObject &a, const Value &b) { return a.bitRShift(b); }
Value bitRShiftEquals(UserObject &a, const Value &b) {
  return a.bitRShiftEquals(b);
}

Value logicalAnd(UserObject &a, const Value &b) { return a.logicalAnd(b); }
Value logicalOr(UserObject &a, const Value &b) { return a.logicalOr(b); }

Value lessThan(UserObject &a, const Value &b) { return a.lessThan(b); }
Value lessEquals(UserObject &a, const Value &b) { return a.lessEquals(b); }
Value greaterThan(UserObject &a, const Value &b) { return a.greaterThan(b); }
Value greaterEquals(UserObject &a, const Value &b) {
  return a.greaterEquals(b);
}

Bool equal(UserObject &a, const Value &b) { return a.equal(b); }
Bool notEqual(UserObject &a, const Value &b) { return a.notEqual(b); }
} // namespace extense::ops
