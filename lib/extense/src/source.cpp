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

#include <iostream>

extense::Source::Char extense::Source::Char::fromStream(std::istream &is) {
  auto c = is.get();
  if (is.eof())
    return afterSource;
  return c;
}

std::ostream &operator<<(std::ostream &os, extense::Source::Char c) {
  if (c.isBeforeSource()) os << "<before source>";
  else if (c.isAfterSource()) os << "<after source>";
  else os << c.get();

  return os;
}

extense::Source::Source(std::istream &is)
    : stream(is), current(Char::BeforeSource()) {}

extense::Source::Char extense::Source::nextChar() {
  if (cache.hasNextChar()) {
    current = Char{cache.nextChar()};
    return current;
  }

  // No more characters
  if (current.isAfterSource()) return current;

  current = Char::fromStream(stream);
  if (current.isAfterSource())
    return current;

  cache.provideChar(current.get());
  return Char{cache.nextChar()};
}

extense::Source::Char extense::Source::backChar() {
  // No previous characters
  if (cache.sourceIndex() <= 0) return Char::BeforeSource();

  if (current.isAfterSource()) {
    // The cache does not have a concept of "out of source" characters, and so
    // its current char is the last character in the source
    current = Char{cache.currentChar()};
    return current;
  }

  return (current = Char{cache.backChar()});
}

extense::Source::Char extense::Source::peekNextChar() {
  if (cache.hasNextChar()) return Char{cache.peekNextChar()};

  // No more characters
  if (current.isAfterSource()) return current;

  auto c = Char::fromStream(stream);
  if (c.isAfterSource()) return Char::AfterSource();

  cache.provideChar(c.get());
  return c;
}

extense::Source::Char extense::Source::peekPreviousChar() {
  if (cache.sourceIndex() > 0)
    return Char{cache.peekPreviousChar()};
  return Char::BeforeSource();
}

std::ostream &operator<<(std::ostream &os, const extense::Source::Location &loc) {
  os << loc.lineNumber() << ':' << loc.linePosition();
  return os;
}
