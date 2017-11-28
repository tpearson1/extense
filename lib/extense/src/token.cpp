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
#include <sstream>

#include <extense/token.h>

static extense::Token fetchNextToken(extense::Source &source);
static void skipWhitespace(extense::Source &source);

std::vector<extense::Token> extense::tokenize(extense::Source &source) {
  std::vector<Token> tokens;
  while (true) {
    auto token = fetchNextToken(source);
    if (token.type() == Token::Type::EndStatement &&
        tokens.back().type() == Token::Type::EndStatement)
      continue; // Collapse multiple EndStatement tokens into one
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
  skipWhitespace(source);
  using extense::Token;

  auto current = source.currentChar();
  if (current.isAfterSource())
    return {source.location(), "", Token::Type::EndSource};

  Token t{source.location()};

#define SINGLE_CHAR_TOKEN(type)                                                \
  t.setText(source.getCharacterSlice());                                       \
  t.setType(Token::Type::type);                                                \
  source.nextChar();                                                           \
  break;

  switch (current.get()) {
  case '\n':
  case ';':
    SINGLE_CHAR_TOKEN(EndStatement)
  default:
    std::ostringstream errorMsg;
    errorMsg << "Unexpected character '" << current.get() << '\'';
    throw extense::LexingError{source.location(), errorMsg.str()};
  }
#undef SINGLE_CHAR_TOKEN

  return t;
}

/*
 * Tries to match a string of characters.
 * The source's current character is the one after the end of the matched string
 * if the match was successful, and is otherwise not changed.
 */
/* static bool tryMatch(extense::Source &source, std::string_view str) { */
/*   for (std::size_t i = 0; i < str.size(); i++) { */
/*     if (str[i] == source.currentChar()) { */
/*       source.nextChar(); */
/*       continue; */
/*     } */

/*     // Rewind back to beginning */
/*     for (std::size_t j = 0; j < i; i++) source.backChar(); */
/*     return false; */
/*   } */

/*   return true; */
/* } */

static void skipWhitespaceOneAttempt(extense::Source &source);

/*
 * Skips all whitespace in the source until the beginning of the next token is
 * reached.
 */
static void skipWhitespace(extense::Source &source) {
  // Continue attempting to skip whitespace until no progress was made
  auto index = source.index();
  do {
    skipWhitespaceOneAttempt(source);
    if (index == source.index()) break;
    index = source.index();
  } while (true);
}

// EOS = End of Source
template <typename Pred>
static void skipPastPermitEOS(extense::Source &source, Pred p) {
  while (!p(source.currentChar()) && source.currentChar().isValidChar())
    source.nextChar();
  source.nextChar();
}

template <typename Pred>
static void skipPast(extense::Source &source, Pred p) {
  skipPastPermitEOS(source, p);
  if (!source.currentChar().isValidChar())
    throw extense::LexingError{source.location(), "Unexpected end of source"};
}

static void skipWhitespaceOneAttempt(extense::Source &source) {
  auto current = source.currentChar();
  if (current == '#') {
    if (source.nextChar() == '{') {
      // Handle multiline comment
      skipPast(source, [](auto c) { return c == '}'; });
    } else {
      // Handle single line comment
      skipPastPermitEOS(source, [](auto c) { return c == '\n'; });
    }
  } else if (current == ' ' || current == '\t') {
    // Should not be an error to end file with whitespace
    skipPastPermitEOS(source, [](auto c) { return c == ' ' || c == '\t'; });
  }
}

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
