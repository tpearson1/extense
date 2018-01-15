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

#include <extense/source.hpp>

#include <ostream>

std::ostream &operator<<(std::ostream &os, extense::Source::Char c) {
  if (c.isBeforeSource())
    os << "<before source>";
  else if (c.isAfterSource())
    os << "<after source>";
  else
    os << c.get();

  return os;
}

extense::Source::Source(std::string source) : data(std::move(source)) {
  lineStartIndices.push_back(0);
}

extense::Source::Char extense::Source::nextChar() {
  if (atLastChar()) {
    idx++;
    return Char::afterSource();
  }

  auto current = currentChar();
  if (current.isBeforeSource()) return Char{data[idx = 0]};
  if (current.isAfterSource()) return current;

  auto c = current.get();
  idx++;
  if (c == '\n') {
    lineIdx++;
    // If we have never passed this newline character before, we need to add the
    // next character's index into the array of line starts
    if (lineCount() == lineIdx) lineStartIndices.push_back(idx);
  }

  return Char{data[idx]};
}

extense::Source::Char extense::Source::backChar() {
  // No previous characters
  if (beforeSource()) return Char::beforeSource();

  if (afterSource()) {
    idx--;
    return currentChar();
  }

  idx--;
  auto curr = currentChar();
  if (curr == '\n') { lineIdx--; }

  return curr;
}

extense::Source::Char extense::Source::peekNextChar() {
  if (atLastChar()) return Char::afterSource();

  auto current = currentChar();
  if (current.isBeforeSource()) return Char{data[0]};
  if (current.isAfterSource()) return current;

  return Char{data[idx + 1]};
}

extense::Source::Char extense::Source::peekPreviousChar() {
  if (beforeSource() || size() == 0) return Char::beforeSource();
  return Char{data[idx - 1]};
}

std::ostream &operator<<(std::ostream &os,
                         const extense::Source::Location &loc) {
  os << loc.lineNumber() << ':' << loc.linePosition();
  return os;
}
