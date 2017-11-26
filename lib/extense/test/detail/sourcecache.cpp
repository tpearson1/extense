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

#include <extense/detail/sourcecache.h>

#include <catch.hpp>

TEST_CASE("General use of SourceCache class", "[SourceCache]") {
  extense::detail::SourceCache cache;

  REQUIRE(!cache.hasNextChar());
  cache.provideChar('c'); // cache = "c"
  REQUIRE(cache.hasNextChar());

  auto c = cache.nextChar();
  REQUIRE(c == 'c');
  REQUIRE(cache.currentChar() == 'c');

  REQUIRE(cache.sourceIndex() == 0);
  REQUIRE(cache.linePosition() == 0);
  REQUIRE(cache.lineIndex() == 0);

  REQUIRE(!cache.hasNextChar());
  cache.provideChar('d'); // cache = "d"
  REQUIRE(cache.hasNextChar());

  REQUIRE(cache.currentChar() == 'c');
  auto c2 = cache.nextChar();
  REQUIRE(c2 == 'd');
  REQUIRE(cache.currentChar() == 'd');

  REQUIRE(cache.sourceIndex() == 1);
  REQUIRE(cache.linePosition() == 1);
  REQUIRE(cache.lineIndex() == 0);

  INFO("Handling newlines");

  cache.provideChar('\n'); // cache = "cd\n"
  cache.nextChar();
  REQUIRE(cache.sourceIndex() == 2);
  REQUIRE(cache.linePosition() == 2);
  REQUIRE(cache.lineIndex() == 0);

  cache.provideChar('a'); // cache = "cd\na"
  cache.nextChar();
  REQUIRE(cache.sourceIndex() == 3);
  REQUIRE(cache.linePosition() == 0);
  REQUIRE(cache.lineIndex() == 1);

  INFO("Split");

  REQUIRE(cache.getSlice(1, 2) == "d");
  REQUIRE(cache.getSlice(1, 3) == "d\n");

  INFO("Back a character");

  auto prev = cache.backChar();
  REQUIRE(prev == '\n');
  REQUIRE(cache.currentChar() == '\n');

  REQUIRE(cache.sourceIndex() == 2);
  REQUIRE(cache.linePosition() == 2);
  REQUIRE(cache.lineIndex() == 0);

  SECTION("peekPreviousChar") {
    auto d = cache.peekPreviousChar();
    REQUIRE(d == 'd');
    REQUIRE(cache.currentChar() == '\n');

    REQUIRE(cache.sourceIndex() == 2);
    REQUIRE(cache.linePosition() == 2);
    REQUIRE(cache.lineIndex() == 0);
  }

  SECTION("peekNextChar") {
    auto n = cache.peekNextChar();
    REQUIRE(n == 'a');
    REQUIRE(cache.currentChar() == '\n');

    REQUIRE(cache.sourceIndex() == 2);
    REQUIRE(cache.linePosition() == 2);
    REQUIRE(cache.lineIndex() == 0);
  }
}
