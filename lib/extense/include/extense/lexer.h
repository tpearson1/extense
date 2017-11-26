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

#ifndef _LIB_EXTENSE__LEXER_H
#define _LIB_EXTENSE__LEXER_H

#include <stdexcept>

#include <extense/token.h>

namespace extense {
/*
 * Tokenizes a text stream.
 */
class Lexer {
  std::vector<Token> tokens;
  Source &source;

  std::vector<Token>::size_type currentTokenIndex = 0;

  /*
   * Returns whether the current token is the last fetched token.
   */
  bool atLastToken() const { return currentTokenIndex + 1 == tokens.size(); }

  /*
   * Returns the last token currently in the list of tokens.
   */
  const Token lastToken() const { return tokens.back(); }

public:
  /*
   * Constructs a Lexer, which is used to tokenize the given Source.
   */
  explicit Lexer(Source &s) : source(s) {
    tokens.emplace_back(source.location(), "", Token::Type::BeginSource);
  }

  /*
   * Returns the current token.
   */
  const Token &currentToken() const { return tokens[currentTokenIndex]; }

  /*
   * Returns the next token, advancing the current token.
   * If the token has not yet been fetched (not cached), then the function may
   * throw an InvalidTokenError exception if the source has invalid text.
   */
  Token nextToken();

  /*
   * Returns the next token without advancing the current token.
   * If the token has not yet been fetched (not cached), then the function may
   * throw an InvalidTokenError exception if the source has invalid text.
   */
  Token peekNextToken();

  /*
   * Goes back a token, changing the current token.
   */
  Token backToken();

  /*
   * Goes back a token without changing the current token.
   */
  Token peekPreviousToken();
};

/*
 * Exception thrown when the Lexer was unable to fetch a token.
 */
class InvalidTokenError : public std::runtime_error {
  Source::Location loc;

public:
  InvalidTokenError(Source::Location location, const std::string &what)
      : std::runtime_error(what), loc(location) {}

  InvalidTokenError(Source::Location location, const char *what)
      : std::runtime_error(what), loc(location) {}

  Source::Location location() const { return loc; }
};

namespace detail {
/*
 * Fetches the next token and adds it to the end of tokens.
 * May throw a InvalidTokenError exception.
 */
Token fetchNextToken(Source &source);

/*
 * Skips all whitespace in the source until the beginning of the next token is
 * reached.
 */
void skipWhitespace(Source &source);
} // namespace detail
} // namespace extense

#endif // _LIB_EXTENSE__LEXER_H
