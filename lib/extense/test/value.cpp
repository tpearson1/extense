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

#include <catch.hpp>
#include <cmath>
#include <sstream>

constexpr const auto floatTolerance = 0.00000001;

bool nearlyEquals(double a, double b) {
  return std::abs(a - b) < floatTolerance;
}

using namespace extense;

TEST_CASE("Storing, accessing and converting Values", "[Value]") {
  Value v{Int{7}};
  REQUIRE(v.is<Int>());
  REQUIRE(v.typeAsString() == "Int");
  REQUIRE(get<Int>(v).value == 7);
  REQUIRE(nearlyEquals(as<Float>(v).value, 7.0));

  Value v2{Float{2.7}};
  REQUIRE(nearlyEquals(get<Float>(v2).value, 2.7));

  bool threw = false;
  try {
    // Should throw because cannot convert Float to Int implicitly
    as<Int>(v2);
  } catch (const InvalidConversion &e) {
    threw = true;
    REQUIRE(e.attemptedImplicit());
  }

  REQUIRE(threw);

  REQUIRE(convertedTo<Int>(v2).value == 2);
}

TEST_CASE("Testing try-conversion functions",
          "[tryConvertImplicitly, tryConvert]") {
  auto f = tryConvertImplicitly<Int, Float>(Int{7});
  REQUIRE(nearlyEquals(f.value, 7.0));

  auto i = tryConvert<Float, Int>(Float{3.2});
  REQUIRE(i.value == 3);

  // Should throw as cannot implicitly convert from Float to Int
  bool threw = false;
  try {
    tryConvertImplicitly<Float, Int>(Float{6.4});
  } catch (const InvalidConversion &e) {
    REQUIRE(e.attemptedImplicit());
    threw = true;
  }
  REQUIRE(threw);

  // Should throw as cannot convert from List to Char
  threw = false;
  try {
    tryConvert<List, Char>(List{Int{7}, Char{'c'}});
  } catch (const InvalidConversion &e) {
    REQUIRE(!e.attemptedImplicit());
    threw = true;
  }
  REQUIRE(threw);
}

TEST_CASE("Visiting values", "[Value, FlatValue]") {
  Value v{Int{7}};
  REQUIRE(get<Int>(v).value == 7);

  visit(
      [](const auto &arg) {
        constexpr const auto isInt =
            std::is_same_v<std::decay_t<decltype(arg)>, Int>;
        REQUIRE(isInt);
        if constexpr (isInt) REQUIRE(arg.value == 7);
      },
      v);

  Value v2{Float{2.5}};

  visit(
      [](const auto &a1, const auto &a2) {
        constexpr const auto a1IsInt =
            std::is_same_v<std::decay_t<decltype(a1)>, Int>;
        constexpr const auto a2IsFloat =
            std::is_same_v<std::decay_t<decltype(a2)>, Float>;
        REQUIRE(a1IsInt);
        REQUIRE(a2IsFloat);
        if constexpr (a1IsInt && a2IsFloat) {
          REQUIRE(a1.value == 7);
          REQUIRE(a2.value == 2.5);
        }
      },
      v, v2);

  INFO("Returning a value");

  auto result = visit([]() { return 7; });
  REQUIRE(result == 7);
}

TEST_CASE("Showing values literally", "[Value, BasicFlatValue, Char, String]") {
  std::ostringstream os;

  // No special behavior
  os << LiteralShow{Int{7}};
  REQUIRE(os.str() == "7");
  os.str("");

  // TODO: Escape sequences for Char and String
  os << LiteralShow{Char{'a'}};
  REQUIRE(os.str() == "`a");
  os.str("");

  os << LiteralShow{String{"Testing"}};
  REQUIRE(os.str() == "\"Testing\"");
  os.str("");

  os << LiteralShow{Int{7}};
  REQUIRE(os.str() == "7");
  os.str("");

  // With Value and FlatValue

  os << LiteralShow{Value{Int{7}}};
  REQUIRE(os.str() == "7");
  os.str("");

  os << LiteralShow{Value{Char{'a'}}};
  REQUIRE(os.str() == "`a");
  os.str("");

  os << LiteralShow{FlatValue{Int{7}}};
  REQUIRE(os.str() == "7");
  os.str("");

  os << LiteralShow{FlatValue{Char{'a'}}};
  REQUIRE(os.str() == "`a");
}

TEST_CASE("typeAsString", "[typeAsString]") {
  REQUIRE(typeAsString<Int> == "Int");
  REQUIRE(typeAsString<String> == "String");
  REQUIRE(typeAsString<Reference> == "Reference");
}

TEST_CASE("Using operators with Value", "[Value]") {}
