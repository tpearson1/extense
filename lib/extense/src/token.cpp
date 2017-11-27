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

static extense::Token fetchNextToken(extense::Source &source);
static void skipWhitespace(extense::Source &source);

std::vector<extense::Token> extense::tokenize(extense::Source &source) {
  std::vector<Token> tokens;
  while (true) {
    skipWhitespace(source);
    auto token = fetchNextToken(source);
    /* if (token.type() == Token::Type::EndStatement && */
    /*     tokens.back().type() == Token::Type::EndStatement) */
    /*   continue; // Collapse multiple EndStatement tokens into one */
    if (token.type() == Token::Type::EndSource) break;

    tokens.push_back(token);
  }

  return tokens;
}

/*
 * Fetches the next token and adds it to the end of tokens.
 * May throw an InvalidTokenError exception.
 */
static extense::Token fetchNextToken(extense::Source &source) {
  using Token = extense::Token;

  if (source.currentChar().isAfterSource())
    return {source.location(), "", Token::Type::EndSource};

  auto t = Token{source.location(), source.getCharacterSlice(),
                 Token::Type::Identifier};
  source.nextChar();
  return t;
}

/*
 * Skips all whitespace in the source until the beginning of the next token is
 * reached.
 */
static void skipWhitespace(extense::Source &source) { (void)source; }

static constexpr const char *const tokenTypeEnumStrings[] = {
#define X(a) #a,
    _LIB_EXTENSE__TOKEN__TYPE_ENUM
#undef X
};

std::ostream &operator<<(std::ostream &os, const extense::Token &token) {
  if (token.type() == extense::Token::Type::EndSource)
    os << "{End source}";
  else {
    os << "{location: " << token.location() << ", text: \"" << token.text()
       << "\", type: " << token.type() << '}';
  }

  return os;
}

std::ostream &operator<<(std::ostream &os, extense::Token::Type type) {
  os << tokenTypeEnumStrings[static_cast<int>(type)];
  return os;
}
