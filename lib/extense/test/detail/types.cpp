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

TEST_CASE("Construction and conversion for types",
          "[ValueTypeBase, Reference, FlatValue, BasicFlatValue, Value]") {
  SECTION("None") {
    // None type contains no state
    auto n = None{};
    (void)n;
    None n2;
    // Explicit conversion to String
    REQUIRE(String{n2}.value == "None");
  }

  SECTION("Int") {
    auto i = Int{7};
    REQUIRE(i.value == 7);
    // Explicit conversion to String
    REQUIRE(String{i}.value == "7");
    // Implicit conversion to Float
    Float f = i;
    REQUIRE(nearlyEquals(f.value, 7.0));
  }

  SECTION("Float") {
    auto f = Float{7.5};
    REQUIRE(f.value == 7.5);
    // Explicit conversion to String
    REQUIRE(String{f}.value == "7.5");
    // Explicit conversion to Int
    auto i = Int{f};
    REQUIRE(i.value == 7);
  }

  SECTION("Bool") {
    auto b = Bool{true};
    REQUIRE(b.value == true);
    // Explicit conversions to String
    REQUIRE(String{b}.value == "true");
    b.value = false;
    REQUIRE(String{b}.value == "false");
  }

  SECTION("Char") {
    auto c = Char{'a'};
    REQUIRE(c.value == 'a');
    // Implicit conversion to String
    String s = c;
    REQUIRE(s.value[0] == 'a');
    REQUIRE(s.value == "a");
  }

  SECTION("String") {
    auto s = String{"Hello, World."};
    REQUIRE(s.value == "Hello, World.");
  }

  SECTION("Set") {
    Set s = {
        // Each entry into a set can have keys and values of differing types
        {String{"set"}, Int{7}},
        {Bool{false}, List{Int{7}, Bool{true}}}};
    // Explicit conversion to String
    REQUIRE(String{s}.value == "{\nfalse -> (7,true)\n\"set\" -> 7\n}");
    auto s2 = Set{{}};
    REQUIRE(String{s2}.value == "{}");
  }

  SECTION("List") {
    auto l = List{Int{3}, String{"Hi"}, Bool{true}};
    // Explicit conversion to String
    REQUIRE(String{l}.value == "(3,\"Hi\",true)");

    // Explicit conversions to list
    auto l2 = List{Int{7}};
    l2 = List{Bool{true}};
    l2 = List{String{"This is a string"}};

    REQUIRE(String{l2}.value == "(\"This is a string\",)");
  }

  SECTION("Reference") {
    SECTION("to an Int") {
      auto v = Value{Int{7}};
      REQUIRE(get<Int>(v).value == 7);
      auto ref = Reference{v};
      REQUIRE(get<Int>(*ref).value == 7);
      *ref = Int{6}; // Implicitly constructs a FlatValue
      REQUIRE(get<Int>(v).value == 6);
      get<Int>(*ref).value = 5;
      REQUIRE(get<Int>(v).value == 5);

      INFO("Wrapping the Reference back into a value");

      auto v2 = Value{ref};
      REQUIRE(get<Int>(v2).value == 5);
      get<Int>(v2).value = 4;
      REQUIRE(get<Int>(v2).value == 4);
      REQUIRE(get<Int>(v).value == 4);
    }

    SECTION("Automatic reference-on-copy types") {
      auto v = Value{String{"Copy"}};
      REQUIRE(get<String>(v).value == "Copy");
      auto ref = Reference{v};
      REQUIRE(get<String>(*ref).value == "Copy");
      *ref = String{"Set"}; // Implicitly constructs Value
      REQUIRE(get<String>(*ref).value == "Set");
      REQUIRE(get<String>(v).value == "Set");

      INFO("Copying String");

      auto v2 = v;
      REQUIRE(get<String>(v2).value == "Set");
      get<String>(v2) = String{"Set again"};
      REQUIRE(get<String>(v2).value == "Set again");
      REQUIRE(get<String>(v).value == "Set again");
      v2 = String{"Set thrice"};
      REQUIRE(get<String>(v2).value == "Set thrice");
      // This assignment constructs a new string and so v1 is unchanged
      REQUIRE(get<String>(v).value == "Set again");
    }
  }
}
