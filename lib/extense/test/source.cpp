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

#include <extense/source.h>

#include <catch.hpp>

TEST_CASE("Basic use of Source class", "[Source]") {
  std::string data = "This is a string";
  extense::Source source{data};

  INFO("Before source");

  auto current = source.backChar();
  REQUIRE(current.isBeforeSource());

  current = source.currentChar();
  REQUIRE(current.isBeforeSource());

  current = source.peekPreviousChar();
  REQUIRE(current.isBeforeSource());

  current = source.backChar();
  REQUIRE(current.isBeforeSource());

  for (std::size_t i = 0; i < data.size(); i++) {
    current = source.nextChar();
    char expected = data[i];
    REQUIRE(current == expected);
    REQUIRE(source.currentChar() == expected);
    REQUIRE(source.linePosition() == i);

    auto loc = source.location();
    REQUIRE(loc.index() == i);
    REQUIRE(loc.linePosition() == i);
    REQUIRE(loc.lineNumber() == 1);
  }

  INFO("Slicing");
  REQUIRE(source.getSlice(3, 7) == "s is");
  REQUIRE(source.getCharacterSlice(3) == "s");

  current = source.nextChar();
  REQUIRE(current.isAfterSource());

  INFO("After source consumed, nextChar should continue to return an after "
       "source character");
  current = source.nextChar();
  REQUIRE(current.isAfterSource());

  REQUIRE(source.peekNextChar().isAfterSource());

  INFO("backChar should now return the last character of the source");
  current = source.backChar();

  REQUIRE(current == 'g');
  REQUIRE(data.size() - 1 == source.linePosition());
  REQUIRE(source.getCharacterSlice() == "g");

  REQUIRE(source.peekNextChar().isAfterSource());
}

TEST_CASE("Newlines with Source", "[Source]") {
  extense::Source source{"1\n12\n1"};

  REQUIRE(source.currentChar() == '1');
  REQUIRE(source.lineNumber() == 1);
  REQUIRE(source.linePosition() == 0);

  source.nextChar();
  REQUIRE(source.currentChar() == '\n');
  REQUIRE(source.lineNumber() == 1);
  REQUIRE(source.linePosition() == 1);

  source.nextChar();
  REQUIRE(source.currentChar() == '1');
  REQUIRE(source.lineNumber() == 2);
  REQUIRE(source.linePosition() == 0);

  source.nextChar();
  REQUIRE(source.currentChar() == '2');
  REQUIRE(source.lineNumber() == 2);
  REQUIRE(source.linePosition() == 1);

  auto loc = source.location();
  REQUIRE(loc.lineNumber() == 2);
  REQUIRE(loc.linePosition() == 1);

  source.backChar();
  REQUIRE(source.currentChar() == '1');
  REQUIRE(source.lineNumber() == 2);
  REQUIRE(source.linePosition() == 0);

  source.backChar();
  REQUIRE(source.currentChar() == '\n');
  REQUIRE(source.lineNumber() == 1);
  REQUIRE(source.linePosition() == 1);

  source.nextChar(); // current = '1'
  source.nextChar(); // current = '2'
  source.nextChar(); // current = '\n'

  REQUIRE(source.currentChar() == '\n');
  REQUIRE(source.lineNumber() == 2);
  REQUIRE(source.linePosition() == 2);

  source.nextChar();
  REQUIRE(source.currentChar() == '1');
  REQUIRE(source.lineNumber() == 3);
  REQUIRE(source.linePosition() == 0);

  source.backChar();
  REQUIRE(source.currentChar() == '\n');
  REQUIRE(source.lineNumber() == 2);
  REQUIRE(source.linePosition() == 2);
}

TEST_CASE("Empty Source", "[Source]") {
  INFO("Need to implement");
  REQUIRE(false);
}
