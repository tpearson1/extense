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

#include <cctype>
#include <ostream>
#include <sstream>

#include <extense/token.h>

std::vector<extense::Token> extense::tokenize(extense::Source &source) {
  std::vector<Token> tokens;
  auto lastTokenIsEndStatement = [&tokens] {
    if (tokens.size() == 0) return false;
    return tokens.back().type() == Token::Type::EndStatement;
  };

  while (true) {
    auto token = detail::fetchNextToken(source);
    if (token.type() == Token::Type::EndStatement && lastTokenIsEndStatement())
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
extense::Token extense::detail::fetchNextToken(extense::Source &source) {
  skipWhitespace(source);
  using extense::Token;

  auto currentChar = source.currentChar();
  if (currentChar.isAfterSource())
    return {source.location(), "", Token::Type::EndSource};

  Token t{source.location()};

#define SINGLE_CHAR_TOKEN(type)                                                \
  {                                                                            \
    t.setType(Token::Type::type);                                              \
    source.nextChar();                                                         \
    break;                                                                     \
  }

#define ONE_TWO_CHAR_TOKENS(c1, t1, c2, t2)                                    \
  {                                                                            \
    constexpr const char bothChars[] = {c1, c2, '\0'};                         \
    if (tryMatch(source, bothChars)) {                                         \
      t.setType(Token::Type::t2);                                              \
      break;                                                                   \
    }                                                                          \
                                                                               \
    if (current == c1) SINGLE_CHAR_TOKEN(t1)                                   \
  }

  auto begin = source.index();
  auto current = currentChar.get();

  switch (current) {
  case '\n':
  case ';': SINGLE_CHAR_TOKEN(EndStatement)
  case ',': SINGLE_CHAR_TOKEN(Comma)
  case '$': SINGLE_CHAR_TOKEN(Dollar)
  case '(': SINGLE_CHAR_TOKEN(LeftParen)
  case ')': SINGLE_CHAR_TOKEN(RightParen)
  case '[': SINGLE_CHAR_TOKEN(LeftBracket)
  case ']': SINGLE_CHAR_TOKEN(RightBracket)
  case '{': SINGLE_CHAR_TOKEN(LeftBrace)
  case '}': SINGLE_CHAR_TOKEN(RightBrace)
  default: {
    // Needs to be before the +/- token checks, because else numbers like +42
    // and -26 would be split into a Plus/Minus token and an Integer token
    if (lexNumber(source, t)) break;

    ONE_TWO_CHAR_TOKENS('=', Assign, '=', Equals)
    ONE_TWO_CHAR_TOKENS('.', Dot, '.', DotDot)
    ONE_TWO_CHAR_TOKENS(':', Colon, ':', ColonColon)
    ONE_TWO_CHAR_TOKENS('+', Plus, '=', PlusEquals)
    ONE_TWO_CHAR_TOKENS('%', Mod, '=', ModEquals)
    ONE_TWO_CHAR_TOKENS('!', Exclamation, '=', NotEquals)
    ONE_TWO_CHAR_TOKENS('&', BitAnd, '=', BitAndEquals)
    ONE_TWO_CHAR_TOKENS('|', BitOr, '=', BitOrEquals)
    ONE_TWO_CHAR_TOKENS('^', BitXor, '=', BitXorEquals)
    ONE_TWO_CHAR_TOKENS('~', BitNot, '=', BitNotEquals)

    if (tryMatch(source, "<=")) {
      t.setType(Token::Type::LessEquals);
      break;
    }

    if (tryMatch(source, "<<=")) {
      t.setType(Token::Type::BitLShiftEquals);
      break;
    }

    if (tryMatch(source, "<<")) {
      t.setType(Token::Type::BitLShift);
      break;
    }

    if (current == '<') SINGLE_CHAR_TOKEN(LessThan)

    if (tryMatch(source, ">=")) {
      t.setType(Token::Type::GreaterEquals);
      break;
    }

    if (tryMatch(source, ">>=")) {
      t.setType(Token::Type::BitRShiftEquals);
      break;
    }

    if (tryMatch(source, ">>")) {
      t.setType(Token::Type::BitRShift);
      break;
    }

    if (current == '>') SINGLE_CHAR_TOKEN(GreaterThan)

    if (tryMatch(source, "-=")) {
      t.setType(Token::Type::MinusEquals);
      break;
    }

    if (tryMatch(source, "->")) {
      t.setType(Token::Type::MapsTo);
      break;
    }

    if (current == '-') SINGLE_CHAR_TOKEN(Minus)

    if (tryMatch(source, "*=")) {
      t.setType(Token::Type::MulEquals);
      break;
    }

    if (tryMatch(source, "**=")) {
      t.setType(Token::Type::PowEquals);
      break;
    }

    if (tryMatch(source, "**")) {
      t.setType(Token::Type::Pow);
      break;
    }

    if (current == '*') SINGLE_CHAR_TOKEN(Mul)

    if (tryMatch(source, "/=")) {
      t.setType(Token::Type::DivEquals);
      break;
    }

    if (tryMatch(source, "//=")) {
      t.setType(Token::Type::FloorDivEquals);
      break;
    }

    if (tryMatch(source, "//")) {
      t.setType(Token::Type::FloorDiv);
      break;
    }

    if (current == '/') SINGLE_CHAR_TOKEN(Div)

    if (tryMatch(source, "and")) {
      t.setType(Token::Type::And);
      break;
    }

    if (tryMatch(source, "or")) {
      t.setType(Token::Type::Or);
      break;
    }

    if (tryMatch(source, "not")) {
      t.setType(Token::Type::Not);
      break;
    }

    if (tryMatch(source, "true")) {
      t.setType(Token::Type::Bool);
      break;
    }

    if (tryMatch(source, "false")) {
      t.setType(Token::Type::Bool);
      break;
    }

    if (lexString(source, t)) break;
    if (lexCharacter(source, t)) break;
    if (lexLabel(source, t)) break;
    if (lexIdentifier(source, t)) break;
    if (lexCustomOperator(source, t)) break;

    std::ostringstream errorMsg;
    errorMsg << "Unexpected character '" << current << '\'';
    throw extense::LexingError{source.location(), errorMsg.str()};
  }
  }
#undef ONE_TWO_CHAR_TOKENS
#undef SINGLE_CHAR_TOKEN

  t.setText(source.getSlice(begin, source.index()));
  return t;
}

bool extense::detail::tryMatch(extense::Source &source, std::string_view str) {
  for (std::size_t i = 0; i < str.size(); i++) {
    if (str[i] == source.currentChar()) {
      source.nextChar();
      continue;
    }

    // Rewind back to beginning
    for (std::size_t j = 0; j < i; j++) source.backChar();
    return false;
  }

  return true;
}

static void skipWhitespaceOneAttempt(extense::Source &source) {
  using namespace extense::detail;

  auto current = source.currentChar();
  if (current == '#') {
    if (source.nextChar() == '{') {
      // Handle multiline comment
      skipPast(source, [](auto c) { return c == '}'; });
    } else {
      // Handle single line comment
      skipPastPermitEOS(source, [](auto c) { return c == '\n'; });
      // We shouldn't consume the newline character because it may be needed as
      // an EndStatement token
      if (!source.currentChar().isAfterSource()) source.backChar();
    }
  } else if (current == ' ' || current == '\t') {
    // Should not be an error to end file with whitespace
    skipPastPermitEOS(source, [](auto c) { return c == ' ' || c == '\t'; });
  }
}

void extense::detail::skipWhitespace(extense::Source &source) {
  // Continue attempting to skip whitespace until no progress was made
  auto index = source.index();
  do {
    skipWhitespaceOneAttempt(source);
    if (index == source.index()) break;
    index = source.index();
  } while (true);
}

bool extense::detail::lexCharacter(extense::Source &source,
                                   extense::Token &out) {
  // TODO: Support for more escape sequences in string and character literals
  auto current = source.currentChar().get();
  if (current != '`') return false;

  if (source.nextChar() == '\\') source.nextChar();

  if (source.currentChar().isAfterSource()) {
    throw extense::LexingError{
        source.location(), "Expected character after '`', not end of source"};
  }

  out.setType(Token::Type::Character);
  source.nextChar();
  return true;
}

bool extense::detail::lexString(extense::Source &source, extense::Token &out) {
  auto current = source.currentChar().get();
  if (current != '\'' && current != '"') return false;

  source.nextChar();
  detail::skipPast(source, [ current, previous = ' ' ](auto c) mutable {
    if (previous == '\\' && c == current) {
      previous = c;
      return false; // Escaped '/" should not end string
    }
    previous = c;
    return c == current;
  });

  out.setType(Token::Type::String);
  return true;
}

bool extense::detail::lexLabel(extense::Source &source, extense::Token &out) {
  auto current = source.currentChar().get();
  if (current != '@') return false;

  if (source.nextChar().isAfterSource()) {
    throw extense::LexingError{
        source.location(), "Expected identifier after '@', not end of source"};
  }

  if (!lexIdentifier(source, out)) {
    throw extense::LexingError{source.location(),
                               "Expected valid identifier after '@'"};
  }

  out.setType(Token::Type::Label);
  return true;
}

static bool safeIsDigit(unsigned char c) { return std::isdigit(c); }

bool extense::detail::lexUnsigned(extense::Source &source) {
  auto begin = source.index();
  skipUntilPermitEOS(source, [](auto c) { return !safeIsDigit(c); });
  return source.index() != begin;
}

static bool lexIntegerHelper(extense::Source &source) {
  auto current = source.currentChar().get();

  if (current == '-' || current == '+')
    source.nextChar();
  else if (!safeIsDigit(current)) return false;

  if (!extense::detail::lexUnsigned(source)) return false;

  return true;
}

bool extense::detail::lexInteger(extense::Source &source, extense::Token &out) {
  bool res = lexIntegerHelper(source);
  if (res) out.setType(Token::Type::Integer);
  return res;
}

bool extense::detail::lexNumber(extense::Source &source, extense::Token &out) {
  if (!lexInteger(source, out)) return false;
  if (source.currentChar() != '.') return true; // Is an integer

  source.nextChar();
  if (!lexUnsigned(source)) {
    // Is not a float
    source.backChar();
    return true;
  }

  out.setType(Token::Type::Float);

  auto current = source.currentChar();
  if (current != 'e' && current != 'E') return true;

  source.nextChar();
  if (!lexIntegerHelper(source)) {
    // Must be an integer exponent
    throw LexingError{source.location(), "Expected integer exponent in float"};
  }

  return true;
}

bool extense::detail::lexIdentifier(extense::Source &source,
                                    extense::Token &out) {
  auto validFirstChar = [](unsigned char c) {
    return std::isalpha(c) || c == '_';
  };

  auto validChar = [validFirstChar](unsigned char c) {
    return validFirstChar(c) || safeIsDigit(c);
  };

  if (!validFirstChar(source.currentChar().get())) return false;

  skipUntilPermitEOS(source, [validChar](auto c) { return !validChar(c); });

  out.setType(Token::Type::Identifier);
  return true;
}

bool extense::detail::lexCustomOperator(extense::Source &source,
                                        extense::Token &out) {
  constexpr const auto permittedChars = "&|^*<>%?";
  // TODO
  (void)source;
  (void)out;
  (void)permittedChars;
  return false;
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
