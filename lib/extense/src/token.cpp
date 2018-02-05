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

#include <algorithm>
#include <array>
#include <cassert>
#include <cctype>
#include <cmath>
#include <ostream>
#include <sstream>

#include <extense/token.hpp>

std::vector<extense::Token> extense::tokenize(Source &source) {
  std::vector<Token> tokens;
  auto lastTokenIsEndStatement = [&tokens] {
    if (tokens.empty()) return false;
    return tokens.back().type() == Token::Type::EndStatement;
  };

  while (true) {
    auto token = detail::fetchNextToken(source);
    if (token.type() == Token::Type::EndStatement && lastTokenIsEndStatement())
      continue; // Collapse multiple EndStatement tokens into one

    tokens.push_back(token);
    if (token.type() == Token::Type::EndSource) break;
  }

  return tokens;
}

/*
 * Fetches the next token and adds it to the end of tokens.
 * May throw an InvalidTokenError exception.
 */
extense::Token extense::detail::fetchNextToken(Source &source) {
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

  auto begin = source.index();
  auto current = currentChar.get();

  switch (current) {
  case '\n':
  case ',': SINGLE_CHAR_TOKEN(EndStatement)
  case '(': SINGLE_CHAR_TOKEN(LeftParen)
  case ')': SINGLE_CHAR_TOKEN(RightParen)
  case '[': SINGLE_CHAR_TOKEN(LeftBracket)
  case ']': SINGLE_CHAR_TOKEN(RightBracket)
  case '{': SINGLE_CHAR_TOKEN(LeftBrace)
  case '}': SINGLE_CHAR_TOKEN(RightBrace)
  default: {
    if (lexOperator(source, t)) break;
    if (lexNumber(source, t)) break;
    if (lexString(source, t)) break;
    if (lexCharacter(source, t)) break;
    if (lexLabel(source, t)) break;
    if (lexIdentifier(source, t)) break;

    std::ostringstream errorMsg;
    errorMsg << "Unexpected character '" << current << '\'';
    throw LexingError{source.location(), errorMsg.str()};
  }
  }
#undef SINGLE_CHAR_TOKEN

  t.setText(source.getSlice(begin, source.index()));
  return t;
}

bool extense::detail::tryMatch(Source &source, std::string_view str) {
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
  }

  if (current == ' ' || current == '\t') {
    // Should not be an error to end file with whitespace
    skipPastPermitEOS(source, [](auto c) { return c == ' ' || c == '\t'; });
  }

  if (current == '\\') {
    // Ignore the \ and the following character
    source.nextChar();
    source.nextChar();
  }
}

void extense::detail::skipWhitespace(Source &source) {
  // Continue attempting to skip whitespace until no progress was made
  auto index = source.index();
  do {
    skipWhitespaceOneAttempt(source);
    if (index == source.index()) break;
    index = source.index();
  } while (true);
}

static bool safeIsDigit(unsigned char c) {
  return static_cast<bool>(std::isdigit(c));
}

constexpr int invalidDigit = -1;

template <typename DigitFunc>
static void lexUnsignedInteger(extense::Source &source, std::int64_t &out,
                               DigitFunc df, int base) {
  int digit;
  // Throw LexingError if first digit is invalid
  if (source.currentChar().isAfterSource() ||
      (digit = df(source.currentChar().get())) == invalidDigit)
    throw extense::LexingError{source.location(), "Expected number"};
  out = digit;

  while (!source.nextChar().isAfterSource()) {
    digit = df(source.currentChar().get());
    if (digit == invalidDigit) return;
    out *= base;
    out += digit;
  }
}

void extense::detail::lexHexadecimalNumber(Source &source, std::int64_t &out) {
  auto hexValue = [](char c) -> int {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'A' && c <= 'F') return (c - 'A') + 10;
    if (c >= 'a' && c <= 'f') return (c - 'f') + 10;
    return invalidDigit;
  };
  lexUnsignedInteger(source, out, hexValue, 16);
}

void extense::detail::lexOctalNumber(Source &source, std::int64_t &out) {
  auto octalValue = [](char c) -> int {
    if (c >= '0' && c <= '7') return c - '0';
    return invalidDigit;
  };
  lexUnsignedInteger(source, out, octalValue, 8);
}

void extense::detail::lexDecimalNumber(Source &source, std::int64_t &out) {
  auto decimalValue = [](char c) -> int {
    if (safeIsDigit(c)) return c - '0';
    return invalidDigit;
  };
  lexUnsignedInteger(source, out, decimalValue, 10);
}

void extense::detail::lexBinaryNumber(Source &source, std::int64_t &out) {
  auto binaryValue = [](char c) -> int {
    if (c == '0') return 0;
    if (c == '1') return 1;
    return invalidDigit;
  };
  lexUnsignedInteger(source, out, binaryValue, 2);
}

template <typename LexFunction>
static void lexEscapeSequenceNumber(extense::Source &source, char &out,
                                    LexFunction lf) {
  std::int64_t hex;
  lf(source, hex);
  if (hex > 255) {
    // The number is too large to store in a character
    throw extense::LexingError{source.location(),
                               "Number number in escape sequence too large"};
  }

  out = static_cast<char>(hex);
}

void extense::detail::lexEscapeSequence(Source &source, char &out) {
  assert(source.currentChar() == '\\');
  if (source.nextChar().isAfterSource()) {
    throw LexingError{source.location(),
                      "Unexpected end of source in escape sequence"};
  }

  auto c = source.currentChar().get();
  switch (c) {
  case '\'': out = '\''; break;
  case '"': out = '"'; break;
  case '\\': out = '\\'; break;
  case 'a': out = '\a'; break;
  case 'b': out = '\b'; break;
  case 'f': out = '\f'; break;
  case 'n': out = '\n'; break;
  case 'r': out = '\r'; break;
  case 't': out = '\t'; break;
  case 'v': out = '\v'; break;
  case 'x': {
    source.nextChar();
    lexEscapeSequenceNumber(source, out, lexHexadecimalNumber);
    return;
  }
  case 'd': {
    source.nextChar();
    lexEscapeSequenceNumber(source, out, lexDecimalNumber);
    return;
  }
  default: {
    if (!safeIsDigit(c))
      throw LexingError{source.location(), "Unrecognized escape sequence"};

    // Since the sequence is a backslash followed by a digit, the escape
    // sequence must be octal
    lexEscapeSequenceNumber(source, out, lexOctalNumber);
    return;
  }
  }

  // To reach here, we must have had a single character escape sequence. We
  // need to skip past that single character.
  source.nextChar();
}

bool extense::detail::lexCharacter(Source &source, Token &out) {
  if (source.currentChar().get() != '`') return false;

  if (source.nextChar() == '\\') {
    char value;
    lexEscapeSequence(source, value);
    out.setType(Token::Type::Character);
    out.setData(value);
    return true;
  }

  if (source.currentChar().isAfterSource()) {
    throw extense::LexingError{
        source.location(), "Expected character after '`', not end of source"};
  }

  out.setType(Token::Type::Character);
  out.setData(source.currentChar().get());
  source.nextChar();
  return true;
}

bool extense::detail::lexString(Source &source, Token &out) {
  auto stringBegin = source.currentChar().get();
  if (stringBegin != '\'' && stringBegin != '"') return false;

  source.nextChar();
  std::string value;
  while (true) {
    if (source.currentChar().isAfterSource()) {
      throw LexingError{source.location(),
                        "Unexpected end of source while lexing String literal"};
    }

    char c = source.currentChar().get();
    if (c == '\\') {
      char processed;
      lexEscapeSequence(source, processed);
      value += processed;
      continue;
    }

    source.nextChar();
    if (c == stringBegin) break;
    value += c;
  }

  out.setType(Token::Type::String);
  out.setData(value);
  return true;
}

bool extense::detail::lexLabel(Source &source, Token &out) {
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

  out.setType(Token::Type::LabelDeclaration);
  return true;
}

static bool lexPossibleSign(extense::Source &source, bool &negative) {
  auto current = source.currentChar().get();
  if (current == '-') {
    negative = true;
    source.nextChar();
    return true;
  }

  if (current == '+') {
    negative = false;
    source.nextChar();
    return true;
  }

  negative = false;
  return safeIsDigit(current);
}

static double powTen(std::int64_t v) {
  double multiplier = 10.0;
  if (v < 0) {
    multiplier = 0.1;
    v *= -1;
  }

  double result = 1.0;
  for (std::int64_t i = 0; i < v; i++) result *= multiplier;
  return result;
}

static double calculateFloatLiteral(std::int64_t wholePart,
                                    std::int64_t decimalPart,
                                    int numDecimalDigits,
                                    std::int64_t exponent) {
  // 12.34 -> 1234
  auto shiftedToRemoveDecimal =
      static_cast<double>(wholePart) * powTen(numDecimalDigits) +
      static_cast<double>(decimalPart);

  auto finalExponent = exponent - numDecimalDigits;
  return shiftedToRemoveDecimal * powTen(finalExponent);
}

// Only lexes positive numbers. Numbers with signs are considered to be two
// tokens: '-/+' and the number.
bool extense::detail::lexNumber(Source &source, Token &out) {
  // We could have a hexadecimal or octal literal, in which case a Float would
  // no longer be possible
  if (source.currentChar().get() == '0') {
    out.setType(Token::Type::Integer);
    std::int64_t value;

    if (source.nextChar() == 'x') {
      // Hexadecimal literal
      source.nextChar();
      lexHexadecimalNumber(source, value);
      out.setData(value);
      return true;
    }

    if (source.currentChar() == 'b') {
      // Binary literal
      source.nextChar();
      lexBinaryNumber(source, value);
      out.setData(value);
      return true;
    }

    // Could just be 0
    if (source.currentChar().isAfterSource() ||
        !safeIsDigit(source.currentChar().get())) {
      out.setData(0L);
      return true;
    }

    // Octal literal
    lexOctalNumber(source, value);
    out.setData(value);
    return true;
  }

  // The token may not be a number. If it does not start with a digit, it must
  // not be.
  if (!safeIsDigit(source.currentChar().get())) return false;

  // At this point the token is either a decimal literal or a Float

  int numDecimalDigits = 0;
  std::int64_t wholePart, decimalPart = 0, exponent;
  lexDecimalNumber(source, wholePart);
  out.setType(Token::Type::Integer);

  if (source.currentChar() == '.') {
    if (!source.nextChar().isAfterSource() &&
        safeIsDigit(source.currentChar().get())) {
      // There is a number after the '.'
      auto firstDecimalDigit = source.index();
      lexDecimalNumber(source, decimalPart);
      numDecimalDigits = source.index() - firstDecimalDigit;
    } else {
      // Go back to make '.' the current character again, as this token is an
      // integer
      source.backChar();
      out.setData(wholePart);
      return true;
    }

    out.setType(Token::Type::Float);
  }

  auto current = source.currentChar();
  if (current != 'e' && current != 'E') {
    if (out.type() == Token::Type::Float) {
      out.setData(
          calculateFloatLiteral(wholePart, decimalPart, numDecimalDigits, 0));
    } else
      out.setData(wholePart);
    return true;
  }

  out.setType(Token::Type::Float);
  source.nextChar();
  bool exponentNegative;
  lexPossibleSign(source, exponentNegative);
  lexDecimalNumber(source, exponent);
  if (exponentNegative) exponent *= -1;
  out.setData(calculateFloatLiteral(wholePart, decimalPart, numDecimalDigits,
                                    exponent));
  return true;
}

static bool lexBool(std::string_view text, extense::Token &out) {
  if (text == "true") {
    out.setType(extense::Token::Type::Bool);
    out.setData(true);
    return true;
  }

  if (text == "false") {
    out.setType(extense::Token::Type::Bool);
    out.setData(false);
    return true;
  }

  return false;
}

#define MATCH_TOKEN(tokenText, type)                                           \
  if (text == std::string_view{tokenText}) {                                   \
    out.setType(extense::Token::Type::type);                                   \
    return true;                                                               \
  }
static bool lexTextualToken(std::string_view text, extense::Token &out) {
  MATCH_TOKEN("and", And);
  MATCH_TOKEN("or", Or);
  MATCH_TOKEN("not", Not);
  MATCH_TOKEN("is", Is);
  MATCH_TOKEN("None", None);
  return false;
}

bool extense::detail::lexIdentifier(Source &source, Token &out) {
  if (source.currentChar().get() == '$') {
    source.nextChar();
    out.setType(Token::Type::Identifier);
    return true;
  }

  auto validFirstChar = [](unsigned char c) {
    return static_cast<bool>(std::isalpha(c)) || c == '_';
  };

  auto validChar = [validFirstChar](unsigned char c) {
    return validFirstChar(c) || safeIsDigit(c);
  };

  if (!validFirstChar(source.currentChar().get())) return false;

  auto begin = source.index();
  skipUntilPermitEOS(source, [validChar](auto c) { return !validChar(c); });

  // Here we check for a bool/textual token
  auto text = source.getSlice(begin, source.index());
  if (lexBool(text, out)) return true;
  if (lexTextualToken(text, out)) return true;

  out.setType(Token::Type::Identifier);
  return true;
}

bool extense::detail::lexOperator(Source &source, Token &out) {
  constexpr const std::array permittedOpChars{'&', '|', '~', '^', '<', '>',
                                              '?', '+', '-', '*', '/', '%',
                                              '!', ':', '.', '='};

  auto isNotValidOpChar = [&](char c) {
    return std::none_of(std::begin(permittedOpChars),
                        std::end(permittedOpChars),
                        [=](char c2) { return c2 == c; });
  };

  auto begin = source.index();
  skipUntilPermitEOS(source, isNotValidOpChar);
  if (begin == source.index()) return false;

  auto text = source.getSlice(begin, source.index());

  // Miscellaneous operators
  MATCH_TOKEN("=", Assign)
  MATCH_TOKEN(".", Dot)
  MATCH_TOKEN("..", DotDot)
  MATCH_TOKEN(":", Colon)
  MATCH_TOKEN("::", ColonColon)
  MATCH_TOKEN("->", MapsTo)
  MATCH_TOKEN("!", Exclamation)

  // Comparison operators
  MATCH_TOKEN("==", Equals)
  MATCH_TOKEN("!=", NotEquals)
  MATCH_TOKEN("<=", LessEquals)
  MATCH_TOKEN(">=", GreaterEquals)
  MATCH_TOKEN("<", LessThan)
  MATCH_TOKEN(">", GreaterThan)

  // Mathematical operators
  MATCH_TOKEN("+", Plus)
  MATCH_TOKEN("-", Minus)
  MATCH_TOKEN("*", Mul)
  MATCH_TOKEN("**", Pow)
  MATCH_TOKEN("/", Div)
  MATCH_TOKEN("//", FloorDiv)
  MATCH_TOKEN("%", Mod)

  MATCH_TOKEN("+=", PlusEquals)
  MATCH_TOKEN("-=", MinusEquals)
  MATCH_TOKEN("*=", MulEquals)
  MATCH_TOKEN("**=", PowEquals)
  MATCH_TOKEN("/=", DivEquals)
  MATCH_TOKEN("//=", FloorDivEquals)
  MATCH_TOKEN("%=", ModEquals)

  // Bitwise operators
  MATCH_TOKEN("&", BitAnd)
  MATCH_TOKEN("|", BitOr)
  MATCH_TOKEN("^", BitXor)
  MATCH_TOKEN("~", BitNot)
  MATCH_TOKEN(">>", BitRShift)
  MATCH_TOKEN("<<", BitLShift)

  MATCH_TOKEN("&=", BitAndEquals)
  MATCH_TOKEN("|=", BitOrEquals)
  MATCH_TOKEN("^=", BitXorEquals)
  MATCH_TOKEN("<<=", BitLShiftEquals)
  MATCH_TOKEN(">>=", BitRShiftEquals)

  // Is otherwise considered to be a custom operator
  out.setType(Token::Type::CustomOperator);
  return true;
}
#undef SINGLE_CHAR_TOKEN

static constexpr std::array tokenTypeEnumStrings{
#define X(a) #a,
    _LIB_EXTENSE__TOKEN__TYPE_ENUM
#undef X
};

std::ostream &extense::operator<<(std::ostream &os, const Token &token) {
  if (token.type() == extense::Token::Type::EndSource)
    os << "{End source}";
  else {
    os << "{location: " << token.location() << ", text: \"" << token.text()
       << "\", type: " << token.type() << '}';
  }

  return os;
}

std::ostream &extense::operator<<(std::ostream &os, Token::Type type) {
  os << tokenTypeEnumStrings.at(static_cast<int>(type));
  return os;
}
