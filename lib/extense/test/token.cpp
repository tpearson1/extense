/*
-------------------------------------------------------------------------------
This file is part of Extense
-------------------------------------------------------------------------------
Copyright (c) 2017 Thomas Pearson

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

#include <ostream>

#include <extense/token.h>

#include <catch.hpp>

TEST_CASE("tryMatch works", "[detail::tryMatch]") {
  using namespace extense;
  Source s{"Find the pattern"};

  bool res = detail::tryMatch(s, "att");
  REQUIRE(!res);
  REQUIRE(s.index() == 0);

  res = detail::tryMatch(s, "Fy");
  REQUIRE(!res);
  REQUIRE(s.index() == 0);

  res = detail::tryMatch(s, "Fi");
  REQUIRE(res);
  REQUIRE(s.index() == 2);
}

TEST_CASE("skipWhitespace works", "[detail::skipWhitespace]") {
  using namespace extense;
  Source s{"\t\t    \t  1        \n   k"};

  detail::skipWhitespace(s);
  REQUIRE(s.currentChar() == '1');

  detail::skipWhitespace(s);
  REQUIRE(s.currentChar() == '1');

  s.nextChar();

  detail::skipWhitespace(s);
  REQUIRE(s.currentChar() == '\n');

  s.nextChar();

  detail::skipWhitespace(s);
  REQUIRE(s.currentChar() == 'k');

  s.nextChar();

  detail::skipWhitespace(s);
  REQUIRE(s.currentChar().isAfterSource());
}

TEST_CASE("Skipping past characters",
          "[detail::skipPastPermitEOS, detail::skipPast]") {
  using namespace extense;

  Source s{"Dummy text"};
  detail::skipPast(s, [](auto c) { return c == 't'; });
  REQUIRE(s.currentChar() == 'e');

  Source s2{"More dummy text"};
  detail::skipPastPermitEOS(s2, [](auto c) { return c == 'z'; });
  REQUIRE(s2.currentChar().isAfterSource());

  SECTION("Testing throwing on failure") {
    Source s3{"abcdefg"};

    bool correct = false;
    try {
      detail::skipPast(s3, [](auto c) { return c == 'h'; });
    } catch (const LexingError &error) { correct = true; }

    REQUIRE(correct);
  }
}

TEST_CASE("Token::Type correct ostream output", "[Token::Type]") {
  std::ostringstream out;
  out << extense::Token::Type::ModEquals;
  REQUIRE(out.str() == "ModEquals");
}
