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

namespace extense::ops {
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

String mul(String a, Int times) {
  mulEquals(a, times);
  return a;
}

template <typename ValueType>
static ValueType &mulEquals(ValueType &a, Int times_) {
  if (times_.value < 0) {
    throw InvalidOperation::Create<ValueType, Int>(
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

constexpr const auto powMessage = "Invalid values for base and/or exponent";

// The power functions consider raising 0 to the 0 as 1
Float pow(Float a, Float b) {
  if (a.value < 0.0 || (a.value == 0.0 && b.value < 0.0))
    throw InvalidOperation::Create<Float, Float>(powMessage);
  return Float{std::pow(a.value, b.value)};
}

Float pow(Float a, Int b) {
  if (a.value == 0.0 && b.value < 0)
    throw InvalidOperation::Create<Float, Int>(powMessage);
  return Float{std::pow(a.value, b.value)};
}

List dotDot(Int a, Int b) {
  if (b.value < a.value) {
    throw InvalidOperation::Create<Int, Int>("Operator '..' requires upper "
                                             "bound to be greater than or "
                                             "equal to lower bound");
  }
  // Should contain b - a + 1 elements (a, a+1, a+2, ... b)
  List::ValueType list(b.value - a.value + 1);
  std::iota(std::begin(list), std::end(list), a);
  return List(list);
}

Reference ref(Value &v) { return Reference{v}; }

Bool equal(const Set &a, const Set &b) { return Bool{a.value == b.value}; }
Bool equal(const List &a, const List &b) { return Bool{a.value == b.value}; }
} // namespace extense::ops

static extense::Value binaryFunction(const extense::String &op, extense::Set &a,
                                     extense::Value &b) {
  return extense::get<extense::Scope>(a[op])(a, b);
}
static extense::Value unaryFunction(const extense::String &op,
                                    extense::Set &a) {
  return extense::get<extense::Scope>(a[op])(a);
}

namespace extense::ops {
Value add(Set &a, Value &b) { return binaryFunction("+"_es, a, b); }
Value addEquals(Set &a, Value &b) { return binaryFunction("+="_es, a, b); }
Value add(Set &a) { return unaryFunction("+"_es, a); }

Value sub(Set &a, Value &b) { return binaryFunction("-"_es, a, b); }
Value subEquals(Set &a, Value &b) { return binaryFunction("-="_es, a, b); }
Value sub(Set &a) { return unaryFunction("-"_es, a); }

Value mul(Set &a, Value &b) { return binaryFunction("*"_es, a, b); }
Value mulEquals(Set &a, Value &b) { return binaryFunction("*="_es, a, b); }

Value div(Set &a, Value &b) { return binaryFunction("/"_es, a, b); }
Value divEquals(Set &a, Value &b) { return binaryFunction("/="_es, a, b); }

Value mod(Set &a, Value &b) { return binaryFunction("%"_es, a, b); }
Value modEquals(Set &a, Value &b) { return binaryFunction("%="_es, a, b); }

Value floorDiv(Set &a, Value &b) { return binaryFunction("//"_es, a, b); }
Value floorDivEquals(Set &a, Value &b) {
  return binaryFunction("//="_es, a, b);
}

Value pow(Set &a, Value &b) { return binaryFunction("**"_es, a, b); }
Value powEquals(Set &a, Value &b) { return binaryFunction("**="_es, a, b); }

Value dotDot(Set &a, Value &b) { return binaryFunction(".."_es, a, b); }

Value bitAnd(Set &a, Value &b) { return binaryFunction("&"_es, a, b); }
Value bitAndEquals(Set &a, Value &b) { return binaryFunction("&="_es, a, b); }

Value bitOr(Set &a, Value &b) { return binaryFunction("|"_es, a, b); }
Value bitOrEquals(Set &a, Value &b) { return binaryFunction("|="_es, a, b); }

Value bitXor(Set &a, Value &b) { return binaryFunction("^"_es, a, b); }
Value bitXorEquals(Set &a, Value &b) { return binaryFunction("^="_es, a, b); }

Value bitNot(Set &a) { return unaryFunction("~"_es, a); }

Value bitLShift(Set &a, Value &b) { return binaryFunction("<<"_es, a, b); }
Value bitLShiftEquals(Set &a, Value &b) {
  return binaryFunction("<<="_es, a, b);
}

Value bitRShift(Set &a, Value &b) { return binaryFunction(">>"_es, a, b); }
Value bitRShiftEquals(Set &a, Value &b) {
  return binaryFunction(">>="_es, a, b);
}

Value lessThan(Set &a, Value &b) { return binaryFunction("<"_es, a, b); }
Value lessEquals(Set &a, Value &b) { return binaryFunction("<="_es, a, b); }
Value greaterThan(Set &a, Value &b) { return binaryFunction(">"_es, a, b); }
Value greaterEquals(Set &a, Value &b) { return binaryFunction(">="_es, a, b); }

Value equal(Set &a, Value &b) { return binaryFunction("=="_es, a, b); }
Value notEqual(Set &a, Value &b) { return binaryFunction("!="_es, a, b); }
} // namespace extense::ops
