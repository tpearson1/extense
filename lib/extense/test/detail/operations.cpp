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

#include "../common.hpp"
#include <catch.hpp>

using namespace extense;
using namespace extense::literals;

TEST_CASE("Operation functions", "[extense::ops]") {
  SECTION("Incrementing and decrementing") {
    Int i{1};
    REQUIRE(ops::inc(i).value == 2);
    REQUIRE(i.value == 2);
    REQUIRE(ops::dec(i).value == 1);
    REQUIRE(i.value == 1);

    Float f{3.5};
    REQUIRE(ops::inc(f).value == 4.5);
    REQUIRE(f.value == 4.5);
    REQUIRE(ops::dec(f).value == 3.5);
    REQUIRE(f.value == 3.5);
  }

  SECTION("Addition") {
    auto isum = ops::add(7_ei, 3_ei);
    REQUIRE(isum.value == 10);
    REQUIRE(ops::addEquals(isum, 4_ei).value == 14);
    REQUIRE(isum.value == 14);

    auto fsum = ops::add(3.5_ef, 4.5_ef);
    REQUIRE(nearlyEquals(fsum.value, 8.0));
    REQUIRE(nearlyEquals(ops::addEquals(fsum, 5.0_ef).value, 13.0));
    REQUIRE(nearlyEquals(fsum.value, 13.0));

    auto ssum = ops::add("Hello, "_es, "World!"_es);
    REQUIRE(ssum.value == "Hello, World!");
    REQUIRE(ops::addEquals(ssum, " I'm here."_es).value ==
            "Hello, World! I'm here.");
    REQUIRE(ssum.value == "Hello, World! I'm here.");

    auto lsum = ops::add(List{3_ei, Bool::f}, List{"Hi"_es});
    REQUIRE(lsum == List{3_ei, Bool::f, "Hi"_es});
    auto expected = List{3_ei, Bool::f, "Hi"_es, 'a'_ec, 1.0_ef};
    REQUIRE(ops::addEquals(lsum, List{'a'_ec, 1.0_ef}) == expected);
    REQUIRE(lsum == expected);
  }

  SECTION("Unary plus") {
    auto i = ops::add(17_ei);
    REQUIRE(i.value == 17);
    i = ops::add(-17_ei);
    REQUIRE(i.value == -17);

    auto f = ops::add(5.0_ef);
    REQUIRE(f.value == 5.0);
    f = ops::add(-5.0_ef);
    REQUIRE(f.value == -5.0);
  }

  SECTION("Unary minus") {
    auto i = ops::sub(6_ei);
    REQUIRE(i.value == -6);
    i = ops::sub(-6_ei);
    REQUIRE(i.value == 6);

    auto f = ops::sub(8.0_ef);
    REQUIRE(f.value == -8.0);
    f = ops::sub(-8.0_ef);
    REQUIRE(f.value == 8.0);
  }

  SECTION("Subtraction") {
    auto idiff = ops::sub(7_ei, 3_ei);
    REQUIRE(idiff.value == 4);
    REQUIRE(ops::subEquals(idiff, 5_ei).value == -1);
    REQUIRE(idiff.value == -1);

    auto fdiff = ops::sub(3.5_ef, 4.5_ef);
    REQUIRE(nearlyEquals(fdiff.value, -1.0));
    REQUIRE(nearlyEquals(ops::subEquals(fdiff, 5.0_ef).value, -6.0));
    REQUIRE(nearlyEquals(fdiff.value, -6.0));
  }

  SECTION("Multiplication") {
    auto iprod = ops::mul(7_ei, 3_ei);
    REQUIRE(iprod.value == 21);
    REQUIRE(ops::mulEquals(iprod, 5_ei).value == 105);
    REQUIRE(iprod.value == 105);

    auto fprod = ops::mul(3.5_ef, 4.5_ef);
    REQUIRE(nearlyEquals(fprod.value, 3.5 * 4.5));
    REQUIRE(nearlyEquals(ops::mulEquals(fprod, Float{1.0 / 3.5}).value, 4.5));
    REQUIRE(nearlyEquals(fprod.value, 4.5));

    auto repeatedString = ops::mul("abc"_es, 4_ei);
    REQUIRE(repeatedString.value == "abcabcabcabc");
    REQUIRE(ops::mulEquals(repeatedString, 0_ei).value == "");
    REQUIRE(repeatedString.value == "");

    auto repeatedList = ops::mul(List{1_ei, Bool::t}, 2_ei);
    auto expected = List{1_ei, Bool::t, 1_ei, Bool::t};
    REQUIRE(repeatedList == expected);
    REQUIRE(ops::mulEquals(repeatedList, 1_ei) == expected);
    REQUIRE(repeatedList == expected);
    REQUIRE(ops::mulEquals(repeatedList, 0_ei) == List{});
    REQUIRE(repeatedList == List{});
  }

  SECTION("Division") {
    auto quotient = ops::div(18.6_ef, 6.0_ef);
    auto expected = 18.6 / 6.0;
    REQUIRE(nearlyEquals(quotient.value, expected));
    expected /= 2.0;
    REQUIRE(nearlyEquals(ops::divEquals(quotient, 2.0_ef).value, expected));
    REQUIRE(nearlyEquals(quotient.value, expected));
  }

  SECTION("Modulo") {
    auto iremainder = ops::mod(83_ei, 17_ei);
    auto iexpected = 83 % 17;
    REQUIRE(iremainder.value == iexpected);
    iexpected %= 4;
    REQUIRE(ops::modEquals(iremainder, 4_ei).value == iexpected);
    REQUIRE(iremainder.value == iexpected);

    auto fremainder = ops::mod(12.4_ef, 3.2_ef);
    auto fexpected = std::fmod(12.4, 3.2);
    REQUIRE(nearlyEquals(fremainder.value, fexpected));
    fexpected = std::fmod(fexpected, 1.3);
    REQUIRE(nearlyEquals(ops::modEquals(fremainder, 1.3_ef).value, fexpected));
    REQUIRE(nearlyEquals(fremainder.value, fexpected));
  }

  SECTION("Floor division") {
    auto iresult = ops::floorDiv(-83_ei, 17_ei);
    REQUIRE(iresult.value == -5);
    REQUIRE(ops::floorDivEquals(iresult, -3_ei).value == 1);
    REQUIRE(iresult.value == 1);

    auto fresult = ops::floorDiv(-37.6_ef, 8.4_ef);
    auto expected = std::floor(-37.6 / 8.4);
    REQUIRE(nearlyEquals(fresult.value, expected));
    expected = std::floor(expected / 1.4);
    REQUIRE(ops::floorDivEquals(fresult, 1.4_ef).value == expected);
    REQUIRE(fresult.value == expected);
  }

  SECTION("Exponentiation") {
    // Float exponent
    auto result = ops::pow(3.9_ef, 2.4_ef);
    auto expected = std::pow(3.9, 2.4);
    REQUIRE(nearlyEquals(result.value, expected));
    expected = std::pow(expected, 0.5);
    REQUIRE(nearlyEquals(ops::powEquals(result, 0.5_ef).value, expected));
    REQUIRE(nearlyEquals(result.value, expected));

    // Int exponent
    result = ops::pow(3.9_ef, 2_ei);
    expected = 3.9 * 3.9;
    REQUIRE(nearlyEquals(result.value, expected));
    expected = 1.0 / expected;
    REQUIRE(nearlyEquals(ops::powEquals(result, -1_ei).value, expected));
    REQUIRE(nearlyEquals(result.value, expected));
  }

  SECTION("Dot dot") {
    REQUIRE(ops::dotDot(-1_ei, 4_ei) ==
            List{-1_ei, 0_ei, 1_ei, 2_ei, 3_ei, 4_ei});
  }

  SECTION("Index") {
    auto l = List{3_ei, Bool::t, 'a'_ec};
    auto v = ops::index(l, 1_ei);
    REQUIRE(v.is<Bool>());
    REQUIRE(get<Bool>(v) == Bool::t);

    auto s = Map{Mapping{"key"_es, "value"_es}, Mapping{'c'_ec, Bool::f},
                 Mapping{3_ei, Bool::t}};
    REQUIRE(ops::index(s, 'c'_ec) == Value{Bool::f});
    REQUIRE(ops::index(s, "key"_es) == Value{"value"_es});
  }

  SECTION("Reference") {
    auto i = Value{42_ei};
    auto ref = ops::ref(i);
    REQUIRE(ref->is<Int>());

    get<Int>(*ref).value = 21;
    REQUIRE(ref->is<Int>());
    REQUIRE(get<Int>(*ref).value == 21);

    REQUIRE(i.is<Int>());
    REQUIRE(get<Int>(i).value == 21);
  }

  SECTION("Bitwise and") {
    //   11011001
    // & 01101101
    // ----------
    //   01001001
    auto i = ops::bitAnd(0b11011001_ei, 0b01101101_ei);
    REQUIRE(i.value == 0b01001001);

    //   01001001
    // & 11000001
    // ----------
    //   01000001
    REQUIRE(ops::bitAndEquals(i, 0b11000001_ei).value == 0b01000001);
    REQUIRE(i.value == 0b01000001);
  }

  SECTION("Bitwise or") {
    //   11011001
    // | 01101101
    // ----------
    //   11111101
    auto i = ops::bitOr(0b11011001_ei, 0b01101101_ei);
    REQUIRE(i.value == 0b11111101);

    //   11111101
    // | 11000001
    // ----------
    //   11111101
    REQUIRE(ops::bitOrEquals(i, 0b11000001_ei).value == 0b11111101);
    REQUIRE(i.value == 0b11111101);
  }

  SECTION("Bitwise xor") {
    //   11011001
    // ^ 01101101
    // ----------
    //   10110100
    auto i = ops::bitXor(0b11011001_ei, 0b01101101_ei);
    REQUIRE(i.value == 0b10110100);

    //   10110100
    // ^ 11000001
    // ----------
    //   01110101
    REQUIRE(ops::bitXorEquals(i, 0b11000001_ei).value == 0b01110101);
    REQUIRE(i.value == 0b01110101);
  }

  SECTION("Bitwise not") {
    //  ~10011000
    // = 01100111
    auto i = ops::bitNot(0b10011000_ei);
    // We only consider the last 8 bits
    REQUIRE((i.value & 0xFF) == 0b01100111);
  }

  // << <<= >> >>=
  SECTION("Bit shifting") {
    auto shifted = ops::bitLShift(1_ei, 3_ei);
    REQUIRE(shifted.value == 8);
    REQUIRE(ops::bitLShiftEquals(shifted, 4_ei).value == 128);
    REQUIRE(shifted.value == 128);

    shifted = ops::bitRShift(24_ei, 2_ei);
    REQUIRE(shifted.value == 6);
    REQUIRE(ops::bitRShiftEquals(shifted, 1_ei).value == 3);
    REQUIRE(shifted.value == 3);
  }

  // and or not
  SECTION("Logical operators") {
    REQUIRE(ops::logicalAnd(Bool::f, Bool::f) == Bool::f);
    REQUIRE(ops::logicalAnd(Bool::f, Bool::t) == Bool::f);
    REQUIRE(ops::logicalAnd(Bool::t, Bool::f) == Bool::f);
    REQUIRE(ops::logicalAnd(Bool::t, Bool::t) == Bool::t);

    REQUIRE(ops::logicalOr(Bool::f, Bool::f) == Bool::f);
    REQUIRE(ops::logicalOr(Bool::f, Bool::t) == Bool::t);
    REQUIRE(ops::logicalOr(Bool::t, Bool::f) == Bool::t);
    REQUIRE(ops::logicalOr(Bool::t, Bool::t) == Bool::t);

    REQUIRE(ops::logicalNot(Bool::f) == Bool::t);
    REQUIRE(ops::logicalNot(Bool::t) == Bool::f);
  }

  // < <= > >= == !=
  SECTION("Comparison operators") {
    REQUIRE(ops::lessThan(7_ei, 14_ei));
    REQUIRE(!ops::lessThan(21_ei, 14_ei));
    REQUIRE(ops::lessThan(3.2_ef, 4.6_ef));
    REQUIRE(!ops::lessThan(13.2_ef, 4.6_ef));

    REQUIRE(ops::lessEquals(7_ei, 14_ei));
    REQUIRE(ops::lessEquals(14_ei, 14_ei));
    REQUIRE(ops::lessEquals(3.2_ef, 4.6_ef));
    REQUIRE(ops::lessEquals(4.5_ef, 4.5_ef));

    REQUIRE(ops::greaterThan(7_ei, 4_ei));
    REQUIRE(!ops::greaterThan(1_ei, 14_ei));
    REQUIRE(ops::greaterThan(13.2_ef, 4.6_ef));
    REQUIRE(!ops::greaterThan(1.2_ef, 4.6_ef));

    REQUIRE(ops::greaterEquals(22_ei, 14_ei));
    REQUIRE(ops::greaterEquals(14_ei, 14_ei));
    REQUIRE(ops::greaterEquals(36.2_ef, 4.6_ef));
    REQUIRE(ops::greaterEquals(4.5_ef, 4.5_ef));

    REQUIRE(ops::equal(none, none));
    REQUIRE(ops::equal(Bool::t, Bool::t));
    REQUIRE(ops::equal(5_ei, 5_ei));
    REQUIRE(ops::equal(3.5_ef, 3.5_ef));
    REQUIRE(ops::equal('c'_ec, 'c'_ec));
    REQUIRE(ops::equal("hello"_es, "hello"_es));
    REQUIRE(ops::equal(List{4_ei, Bool::t}, List{4.0_ef, Bool::t}));
    auto s = Map{Mapping{'a'_ec, "hi"_es}, Mapping{Bool::t, 3_ei}};
    REQUIRE(ops::equal(s, s));

    REQUIRE(ops::equal(Value{2_ei}, Value{2_ei}));
    REQUIRE(ops::equal(Value{2_ei}, Value{2.0_ef}));

    REQUIRE(ops::notEqual(3_ei, 17_ei));
  }
}

TEST_CASE("Operations via C++ operators", "[extense::ops]") {
  // The operators just use the named types in the extense::ops namespace, so
  // comprehensive testing is unnecessary
  REQUIRE(!Bool::f);
  REQUIRE(7_ei + 3_ei == 10_ei);
  REQUIRE(3_ei < 29_ei);
}
