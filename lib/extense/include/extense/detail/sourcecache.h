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

#ifndef _LIB_EXTENSE_DETAIL__SOURCE_CACHE_H
#define _LIB_EXTENSE_DETAIL__SOURCE_CACHE_H

#include <cassert>
#include <string>
#include <string_view>
#include <vector>

namespace extense::detail {
/*
 * Stores characters in a source which have already been read.
 */
class SourceCache {
  std::string source;

  // Stores the indices in the source which represent the first characters on a
  // line
  std::vector<std::string::size_type> lineStartIndices;

  int lineIdx = 0;

  // Index of the current character in the source file.
  // It starts off at -1 so when the first character is read, the index is 0.
  int index = -1;

  /*
   * The index of the character in the source file which is the first character
   * on line lineIndex.
   */
  int getLineStartIndex(int lineIndex) const {
    return lineStartIndices[lineIndex];
  }

public:
  SourceCache() {
    lineStartIndices.push_back(0);
  }

  /*
   * Gets the cache's current size.
   * This changes when characters are provided.
   */
  int size() const { return static_cast<int>(source.size()); }

  /*
   * Creates a string_view which refers to a portion of the cached source.
   * Begin is the first character to include and end is the character after the
   * last included.
   *
   * The slice can only be obtained if all required characters have been
   * provided.
   *
   * For example, if the cache contains "hello", slicing with begin=1, end=3,
   * would give "el".
   */
  std::string_view getSlice(int begin, int end) {
    assert(end > begin);
    assert(size() >= end); // Haven't provided enough characters
    return {&source[begin], static_cast<std::size_t>(end - begin)};
  }

  /*
   * Gets the number of lines cached.
   * This may change when characters are provided.
   */
  int lineCount() const { return static_cast<int>(lineStartIndices.size()); }

  /*
   * Returns the index of the current character in the source.
   */
  int sourceIndex() const { return index; }

  /*
   * Reserves space in the cache.
   */
  void reserve(std::string::size_type size) { source.reserve(size); }

  /*
   * Gets the index of the current line.
   */
  int lineIndex() const { return lineIdx; }

  /*
   * Gets the index of the current character in its line.
   * The index 0 is the first character in a line.
   */
  int linePosition() const { return index - getLineStartIndex(lineIdx); }

  /*
   * Gets the currently active character.
   */
  char currentChar() const { return source[index]; }

  /*
   * Returns whether or not the cached source has the next character currently
   * stored.
   */
  bool hasNextChar() const { return size() != index + 1; }

  /*
   * Provides the next character in the source to be cached.
   * The next character must not already be cached.
   *
   * Note: This function does not call nextChar, so the current character is
   * unchanged, and calling nextChar will return the provided character.
   */
  void provideChar(char c) {
    assert(!hasNextChar());
    source.push_back(c);
  }

  /*
   * Gets and returns the next character, which then becomes the current
   * character.
   * The next character must have already been provided.
   */
  char nextChar();

  /*
   * Gets and returns the previous character, which then becomes the current
   * character.
   * The function expects there to be a previous character, so this function
   * cannot be called when the current character is the first character in the
   * file.
   */
  char backChar();

  /*
   * Gets and returns the next character, without changing the current
   * character.
   * The next character must have already been provided.
   */
  char peekNextChar() const {
    assert(hasNextChar());
    return source[index + 1];
  }

  /*
   * Gets and returns the previous character, without changing the current
   * character.
   * The function must not be called when the current character is the first
   * in the source.
   */
  char peekPreviousChar() const {
    assert(index > 0);
    return source[index - 1];
  }
};
} // namespace extense::detail

#endif // _LIB_EXTENSE_DETAIL__SOURCE_CACHE_H
