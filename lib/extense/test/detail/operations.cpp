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

#include <extense/value.h>

#include "../common.h"
#include <catch.hpp>

using namespace extense;

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
    auto isum = ops::add(Int{7}, Int{3});
    REQUIRE(isum.value == 10);
    REQUIRE(ops::addEquals(isum, Int{4}).value == 14);
    REQUIRE(isum.value == 14);

    auto fsum = ops::add(Float{3.5}, Float{4.5});
    REQUIRE(nearlyEquals(fsum.value, 8.0));
    REQUIRE(nearlyEquals(ops::addEquals(fsum, Float{5.0}).value, 13.0));
    REQUIRE(nearlyEquals(fsum.value, 13.0));

    auto ssum = ops::add(String{"Hello, "}, String{"World!"});
    REQUIRE(ssum.value == "Hello, World!");
    REQUIRE(ops::addEquals(ssum, String{" I'm here."}).value ==
            "Hello, World! I'm here.");
    REQUIRE(ssum.value == "Hello, World! I'm here.");

    auto lsum = ops::add(List{Int{3}, Bool{false}}, List{String{"Hi"}});
    REQUIRE(lsum == List{Int{3}, Bool{false}, String{"Hi"}});
    List expected = {Int{3}, Bool{false}, String{"Hi"}, Char{'a'}, Float{1.0}};
    REQUIRE(ops::addEquals(lsum, List{Char{'a'}, Float{1.0}}) == expected);
    REQUIRE(lsum == expected);
  }

  SECTION("Unary plus") {
    auto i = ops::add(Int{17});
    REQUIRE(i.value == 17);
    i = ops::add(Int{-17});
    REQUIRE(i.value == -17);

    auto f = ops::add(Float{5.0});
    REQUIRE(f.value == 5.0);
    f = ops::add(Float{-5.0});
    REQUIRE(f.value == -5.0);
  }

  SECTION("Unary minus") {
    auto i = ops::sub(Int{6});
    REQUIRE(i.value == -6);
    i = ops::sub(Int{-6});
    REQUIRE(i.value == 6);

    auto f = ops::sub(Float{8.0});
    REQUIRE(f.value == -8.0);
    f = ops::sub(Float{-8.0});
    REQUIRE(f.value == 8.0);
  }

  SECTION("Subtraction") {
    auto idiff = ops::sub(Int{7}, Int{3});
    REQUIRE(idiff.value == 4);
    REQUIRE(ops::subEquals(idiff, Int{5}).value == -1);
    REQUIRE(idiff.value == -1);

    auto fdiff = ops::sub(Float{3.5}, Float{4.5});
    REQUIRE(nearlyEquals(fdiff.value, -1.0));
    REQUIRE(nearlyEquals(ops::subEquals(fdiff, Float{5.0}).value, -6.0));
    REQUIRE(nearlyEquals(fdiff.value, -6.0));
  }

  SECTION("Multiplication") {
    auto iprod = ops::mul(Int{7}, Int{3});
    REQUIRE(iprod.value == 21);
    REQUIRE(ops::mulEquals(iprod, Int{5}).value == 105);
    REQUIRE(iprod.value == 105);

    auto fprod = ops::mul(Float{3.5}, Float{4.5});
    REQUIRE(nearlyEquals(fprod.value, 3.5 * 4.5));
    REQUIRE(nearlyEquals(ops::mulEquals(fprod, Float{1.0 / 3.5}).value, 4.5));
    REQUIRE(nearlyEquals(fprod.value, 4.5));

    auto repeatedString = ops::mul(String{"abc"}, Int{4});
    REQUIRE(repeatedString.value == "abcabcabcabc");
    REQUIRE(ops::mulEquals(repeatedString, Int{0}).value == "");
    REQUIRE(repeatedString.value == "");

    auto repeatedList = ops::mul(List{Int{1}, Bool{true}}, Int{2});
    auto expected = List{Int{1}, Bool{true}, Int{1}, Bool{true}};
    REQUIRE(repeatedList == expected);
    REQUIRE(ops::mulEquals(repeatedList, Int{1}) == expected);
    REQUIRE(repeatedList == expected);
    REQUIRE(ops::mulEquals(repeatedList, Int{0}) == List{});
    REQUIRE(repeatedList == List{});
  }

  SECTION("Division") {
    auto quotient = ops::div(Float{18.6}, Float{6.0});
    auto expected = 18.6 / 6.0;
    REQUIRE(nearlyEquals(quotient.value, expected));
    expected /= 2.0;
    REQUIRE(nearlyEquals(ops::divEquals(quotient, Float{2.0}).value, expected));
    REQUIRE(nearlyEquals(quotient.value, expected));
  }

  SECTION("Modulo") {
    auto iremainder = ops::mod(Int{83}, Int{17});
    auto iexpected = 83 % 17;
    REQUIRE(iremainder.value == iexpected);
    iexpected %= 4;
    REQUIRE(ops::modEquals(iremainder, Int{4}).value == iexpected);
    REQUIRE(iremainder.value == iexpected);

    auto fremainder = ops::mod(Float{12.4}, Float{3.2});
    auto fexpected = std::fmod(12.4, 3.2);
    REQUIRE(nearlyEquals(fremainder.value, fexpected));
    fexpected = std::fmod(fexpected, 1.3);
    REQUIRE(
        nearlyEquals(ops::modEquals(fremainder, Float{1.3}).value, fexpected));
    REQUIRE(nearlyEquals(fremainder.value, fexpected));
  }

  SECTION("Floor division") {
    auto iresult = ops::floorDiv(Int{-83}, Int{17});
    REQUIRE(iresult.value == -5);
    REQUIRE(ops::floorDivEquals(iresult, Int{-3}).value == 1);
    REQUIRE(iresult.value == 1);

    auto fresult = ops::floorDiv(Float{-37.6}, Float{8.4});
    auto expected = std::floor(-37.6 / 8.4);
    REQUIRE(nearlyEquals(fresult.value, expected));
    expected = std::floor(expected / 1.4);
    REQUIRE(ops::floorDivEquals(fresult, Float{1.4}).value == expected);
    REQUIRE(fresult.value == expected);
  }

  SECTION("Exponentiation") {
    // Float exponent
    auto result = ops::pow(Float{3.9}, Float{2.4});
    auto expected = std::pow(3.9, 2.4);
    REQUIRE(nearlyEquals(result.value, expected));
    expected = std::pow(expected, 0.5);
    REQUIRE(nearlyEquals(ops::powEquals(result, Float{0.5}).value, expected));
    REQUIRE(nearlyEquals(result.value, expected));

    // Int exponent
    result = ops::pow(Float{3.9}, Int{2});
    expected = 3.9 * 3.9;
    REQUIRE(nearlyEquals(result.value, expected));
    expected = 1.0 / expected;
    REQUIRE(nearlyEquals(ops::powEquals(result, Int{-1}).value, expected));
    REQUIRE(nearlyEquals(result.value, expected));
  }

  // TODO

  SECTION("Dot dot") {
    REQUIRE(ops::dotDot(Int{-1}, Int{4}) ==
            List{Int{-1}, Int{0}, Int{1}, Int{2}, Int{3}, Int{4}});
  }

  SECTION("Index") {
    List l = {Int{3}, Bool::t, Char{'a'}};
    auto v = ops::index(l, Int{1});
    REQUIRE(v.is<Bool>());
    REQUIRE(get<Bool>(v) == Bool::t);

    Set s = {{String{"key"}, String{"value"}},
             {Char{'c'}, Bool::f},
             {Int{3}, Bool::t}};
    REQUIRE(ops::index(s, Char{'c'}) == Bool::f);
    REQUIRE(ops::index(s, String{"key"}) == String{"value"});
  }

  SECTION("Reference") {
    Value i = Int{42};
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
    auto i = ops::bitAnd(Int{0b11011001}, Int{0b01101101});
    REQUIRE(i.value == 0b01001001);

    //   01001001
    // & 11000001
    // ----------
    //   01000001
    REQUIRE(ops::bitAndEquals(i, Int{0b11000001}).value == 0b01000001);
    REQUIRE(i.value == 0b01000001);
  }

  SECTION("Bitwise or") {
    //   11011001
    // | 01101101
    // ----------
    //   11111101
    auto i = ops::bitOr(Int{0b11011001}, Int{0b01101101});
    REQUIRE(i.value == 0b11111101);

    //   11111101
    // | 11000001
    // ----------
    //   11111101
    REQUIRE(ops::bitOrEquals(i, Int{0b11000001}).value == 0b11111101);
    REQUIRE(i.value == 0b11111101);
  }

  SECTION("Bitwise xor") {
    //   11011001
    // ^ 01101101
    // ----------
    //   10110100
    auto i = ops::bitXor(Int{0b11011001}, Int{0b01101101});
    REQUIRE(i.value == 0b10110100);

    //   10110100
    // ^ 11000001
    // ----------
    //   01110101
    REQUIRE(ops::bitXorEquals(i, Int{0b11000001}).value == 0b01110101);
    REQUIRE(i.value == 0b01110101);
  }

  SECTION("Bitwise not") {
    //  ~10011000
    // = 01100111
    auto i = ops::bitNot(Int{0b10011000});
    // We only consider the last 8 bits
    REQUIRE((i.value & 0xFF) == 0b01100111);
  }

  // << <<= >> >>=
  SECTION("Bit shifting") {
    auto shifted = ops::bitLShift(Int{1}, Int{3});
    REQUIRE(shifted.value == 8);
    REQUIRE(ops::bitLShiftEquals(shifted, Int{4}).value == 128);
    REQUIRE(shifted.value == 128);

    shifted = ops::bitRShift(Int{24}, Int{2});
    REQUIRE(shifted.value == 6);
    REQUIRE(ops::bitRShiftEquals(shifted, Int{1}).value == 3);
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
    REQUIRE(ops::lessThan(Int{7}, Int{14}));
    REQUIRE(!ops::lessThan(Int{21}, Int{14}));
    REQUIRE(ops::lessThan(Float{3.2}, Float{4.6}));
    REQUIRE(!ops::lessThan(Float{13.2}, Float{4.6}));

    REQUIRE(ops::lessEquals(Int{7}, Int{14}));
    REQUIRE(ops::lessEquals(Int{14}, Int{14}));
    REQUIRE(ops::lessEquals(Float{3.2}, Float{4.6}));
    REQUIRE(ops::lessEquals(Float{4.5}, Float{4.5}));

    REQUIRE(ops::greaterThan(Int{7}, Int{4}));
    REQUIRE(!ops::greaterThan(Int{1}, Int{14}));
    REQUIRE(ops::greaterThan(Float{13.2}, Float{4.6}));
    REQUIRE(!ops::greaterThan(Float{1.2}, Float{4.6}));

    REQUIRE(ops::greaterEquals(Int{22}, Int{14}));
    REQUIRE(ops::greaterEquals(Int{14}, Int{14}));
    REQUIRE(ops::greaterEquals(Float{36.2}, Float{4.6}));
    REQUIRE(ops::greaterEquals(Float{4.5}, Float{4.5}));

    REQUIRE(ops::equal(None{}, None{}));
    REQUIRE(ops::equal(Bool::t, Bool::t));
    REQUIRE(ops::equal(Int{5}, Int{5}));
    REQUIRE(ops::equal(Float{3.5}, Float{3.5}));
    REQUIRE(ops::equal(Char{'c'}, Char{'c'}));
    REQUIRE(ops::equal(String{"hello"}, String{"hello"}));
    REQUIRE(ops::equal(List{Int{4}, Bool::t}, List{Float{4.0}, Bool::t}));
    auto s = Set{{Char{'a'}, String{"hi"}}, {Bool::t, Int{3}}};
    REQUIRE(ops::equal(s, s));

    REQUIRE(ops::equal(Value{Int{2}}, Value{Int{2}}));
    REQUIRE(ops::equal(Value{Int{2}}, Value{Float{2.0}}));

    REQUIRE(ops::notEqual(Int{3}, Int{17}));
  }
}

TEST_CASE("Operations via C++ operators", "[extense::ops]") {
  // The operators just use the named types in the extense::ops namespace, so
  // comprehensive testing is unnecessary
  REQUIRE(!Bool::f);
  REQUIRE(Int{7} + Int{3} == Int{10});
  REQUIRE(Int{3} < Int{29});
}
