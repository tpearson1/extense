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

#ifndef _LIB_EXTENSE__SOURCE_H
#define _LIB_EXTENSE__SOURCE_H

#include <iosfwd>

#include <extense/detail/sourcecache.h>

namespace extense {
class Source {
public:
  struct Char {
    /*
     * Returns whether the character represents a character before or after the
     * source respectively.
     */
    bool isBeforeSource() const { return value == beforeSource; }
    bool isAfterSource() const { return value == afterSource; }

    /*
     * Is the character actually a valid character (not before or after source).
     */
    bool isValidChar() const { return !(isBeforeSource() || isAfterSource()); }

    /*
     * Gets the character as a char. Must be a valid character.
     */
    char get() const {
      assert(isValidChar());
      return static_cast<char>(value);
    }

    /*
     * Constructs characters.
     */
    explicit Char(char val) : value(val) {}
    static Char BeforeSource() { return beforeSource; }
    static Char AfterSource() { return afterSource; }

    /*
     * Gets a character from a stream.
     * May be an 'after source' character if the istream reads eof.
     */
    static Char fromStream(std::istream &is);

    friend bool operator==(const Char &lhs, char rhs) {
      return lhs.value == static_cast<int>(rhs);
    }

    friend bool operator!=(const Char &lhs, char rhs) {
      return lhs.value != static_cast<int>(rhs);
    }

  private:
    Char(int val) : value(val) {}

    int value;

    // Characters outside of the range of the source
    static constexpr int beforeSource = 256;
    static constexpr int afterSource = 257;
  };

private:
  std::istream &stream;

  detail::SourceCache cache;
  Char current;

  friend class Char;

public:
  /*
   * Constructs a Source with the given input stream.
   */
  explicit Source(std::istream &is);

  /*
   * Advances the current character to the next in the source, returning the new
   * current character.
   */
  Char nextChar();

  /*
   * Changes the current character to the character before it, and returns the
   * new current character.
   */
  Char backChar();

  /*
   * Returns the current character.
   */
  Char currentChar() const { return current; }

  /*
   * Return the character after current, without changing the current character.
   */
  Char peekNextChar();

  /*
   * Return the character before current, without changing the current
   * character.
   */
  Char peekPreviousChar();

  /*
   * The index for the current character in the source.
   */
  int index() const { return cache.sourceIndex(); }

  /*
   * The current line number (starts at one).
   */
  int lineNumber() const { return cache.lineIndex() + 1; }

  /*
   * The current index in the current line.
   */
  int linePosition() const { return cache.linePosition(); }

  class Location {
#ifndef NDEBUG
    const Source *owner; // Used for error checking when in debug
#endif

    int idx;
    int lineNum;
    int linePos;

#ifdef NDEBUG
    Location(int index, int lineNumber, int linePosition)
        : idx(index), lineNum(lineNumber), linePos(linePosition) {}
#else
    Location(const Source *o, int index, int lineNumber, int linePosition)
        : owner(o), idx(index), lineNum(lineNumber), linePos(linePosition) {}
#endif

  public:
    int index() const { return idx; }
    int lineNumber() const { return lineNum; }
    int linePosition() const { return linePos; }

    bool operator==(const Location &other) const {
      // Comparing locations from two different sources is not permitted
      assert(owner == other.owner);

      return idx == other.idx;
    }

    friend class Source;
  };

  /*
   * Returns a Location instance for the current character.
   */
#ifdef NDEBUG
  Location location() { return {index(), lineNumber(), linePosition()}; }
#else
  Location location() { return {this, index(), lineNumber(), linePosition()}; }
#endif

  /*
   * Creates a string_view which references a portion of the source.
   * Begin is the first character to include and end is the character after the
   * last included.
   *
   * The slice can only be obtained if the source has been traversed to the end
   * of the slice.
   *
   * For example, if the cache contains "hello", slicing with begin=1, end=3,
   * would give "el".
   */

  std::string_view getSlice(int beginIndex, int endIndex) {
    return cache.getSlice(beginIndex, endIndex);
  }

  std::string_view getSlice(const Location &begin, const Location &end) {
    return cache.getSlice(begin.index(), end.index());
  }
};
} // namespace extense

std::ostream &operator<<(std::ostream &, extense::Source::Char);
std::ostream &operator<<(std::ostream &, const extense::Source::Location &);

#endif // _LIB_EXTENSE__SOURCE_H
