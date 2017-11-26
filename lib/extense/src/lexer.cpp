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

#include <extense/lexer.h>

extense::Token extense::Lexer::nextToken() {
  if (!atLastToken()) {
    currentTokenIndex++;
    return currentToken();
  }

  if (lastToken().type() != Token::Type::EndSource) {
    tokens.emplace_back(detail::fetchNextToken(source));
    currentTokenIndex++;
  }

  return lastToken();
}

extense::Token extense::Lexer::peekNextToken() {
  if (!atLastToken()) return tokens[currentTokenIndex + 1];

  if (lastToken().type() != Token::Type::EndSource)
    tokens.emplace_back(detail::fetchNextToken(source));
  return lastToken();
}

extense::Token extense::Lexer::backToken() {
  if (currentTokenIndex == 0) return currentToken();

  currentTokenIndex--;
  return currentToken();
}

extense::Token extense::Lexer::peekPreviousToken() {
  if (currentTokenIndex == 0) return currentToken();

  return tokens[currentTokenIndex - 1];
}

extense::Token extense::detail::fetchNextToken(extense::Source &source) {
  skipWhitespace(source);
  // TODO
  source.nextChar();
  if (source.currentChar().isAfterSource())
    return {source.location(), "", Token::Type::EndSource};
  return {source.location(),
          source.getSlice(source.index(), source.index() + 1),
          Token::Type::Identifier};
}

void extense::detail::skipWhitespace(extense::Source & /*source*/) {
  // TODO
}
