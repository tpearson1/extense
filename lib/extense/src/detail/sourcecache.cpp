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

char extense::detail::SourceCache::nextChar() {
  assert(hasNextChar());

  // Character which was current before this call changes that
  auto oldCurrent = currentChar();
  index++;

  if (oldCurrent == '\n') {
    lineIdx++;
    // If we have never passed this newline character before, we need to add the
    // next character's index into the array of line starts
    if (lineCount() == lineIdx)
      lineStartIndices.push_back(index);
  }

  // Return the new current character
  return currentChar();
}

char extense::detail::SourceCache::backChar() {
  assert(index > 0);

  if (linePosition() == 0) {
    // Going back a character will decrease the line count.
    lineIdx--;
  }

  index--;
  return currentChar();
}
