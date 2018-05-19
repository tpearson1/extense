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

#ifndef _LIB_EXTENSE__TOKEN_HPP
#define _LIB_EXTENSE__TOKEN_HPP

#include <cstdint>
#include <iosfwd>
#include <variant>

#include <extense/exception.hpp>
#include <extense/source.hpp>

#define _LIB_EXTENSE__TOKEN__TYPE_ENUM                                         \
  /****** START OF BINARY OPERATORS ******/                                    \
  X(CustomOperator) /*   E.g. <=> or |>   */                                   \
  X(Semicolon) /*   ;   */                                                     \
  X(Colon) /*   :   */                                                         \
                                                                               \
  X(Assign) /*   =   */                                                        \
                                                                               \
  /* Relative math Operators */                                                \
  X(PlusEquals) /*   +=   */                                                   \
  X(MinusEquals) /*   -=   */                                                  \
  X(MulEquals) /*   *=   */                                                    \
  X(DivEquals) /*   /=   */                                                    \
  X(FloorDivEquals) /*   //=   */                                              \
  X(PowEquals) /*   **=   */                                                   \
  X(ModEquals) /*   %=   */                                                    \
                                                                               \
  /* Relative bitwise Operators */                                             \
  X(BitAndEquals) /*   &=   */                                                 \
  X(BitOrEquals) /*   |=   */                                                  \
  X(BitXorEquals) /*   ^=   */                                                 \
  X(BitLShiftEquals) /*   <<=   */                                             \
  X(BitRShiftEquals) /*   >>=   */                                             \
                                                                               \
  /* Binary Bitwise operators */                                               \
  X(BitAnd) /*   &   */                                                        \
  X(BitOr) /*   |   */                                                         \
  X(BitXor) /*   ^   */                                                        \
  X(BitLShift) /*   <<   */                                                    \
  X(BitRShift) /*   >>   */                                                    \
                                                                               \
  /* Binary Logical operators */                                               \
  X(And) /*   and   */                                                         \
  X(Or) /*   or   */                                                           \
                                                                               \
  /* Comparison operators */                                                   \
  X(Equals) /*   ==   */                                                       \
  X(NotEquals) /*   !=   */                                                    \
  X(LessThan) /*   <   */                                                      \
  X(LessEquals) /*   <=   */                                                   \
  X(GreaterThan) /*   >   */                                                   \
  X(GreaterEquals) /*   >=   */                                                \
                                                                               \
  X(Dot) /*   .   */                                                           \
  X(SemicolonSemicolon) /*   ;;   */                                           \
  X(ColonColon) /*   ::   */                                                   \
                                                                               \
  /* Querying the type of a variable */                                        \
  X(Is) /*   is   */                                                           \
                                                                               \
  X(DotDot) /*   ..   */                                                       \
                                                                               \
  /* Math operators */                                                         \
  X(Mul) /*   *   */                                                           \
  X(FloorDiv) /*   //   */                                                     \
  X(Pow) /*   **   */                                                          \
  X(Mod) /*   %   */                                                           \
  /****** START OF UNARY OPERATORS ******/                                     \
  X(Div) /*   /   */                                                           \
  X(Plus) /*   +   */                                                          \
  X(Minus) /*   -   */                                                         \
  /****** END OF BINARY OPERATORS ******/                                      \
                                                                               \
  X(Not) /*   not   */                                                         \
  X(Exclamation) /*   !   */                                                   \
  X(BitNot) /*   ~   */                                                        \
  /****** END OF UNARY OPERATORS ******/                                       \
                                                                               \
  /****** BEGIN TERMINATES EXPRESSION ******/                                  \
  X(EndStatement) /*   , or a newline   */                                     \
  X(MapsTo) /* -> */                                                           \
                                                                               \
  /* End of source */                                                          \
  X(EndSource)                                                                 \
                                                                               \
  X(RightParen) /*   )   */                                                    \
  X(RightBracket) /*   ]   */                                                  \
  X(RightBrace) /*   }   */                                                    \
  /****** END TERMINATES EXPRESSION ******/                                    \
                                                                               \
  X(LeftParen) /*   (   */                                                     \
  X(LeftBracket) /*   [   */                                                   \
  X(LeftBrace) /*   {   */                                                     \
                                                                               \
  /* Literals */                                                               \
  X(Integer) /*   E.g. 1234   */                                               \
  X(Bool) /*   Either 'true' or 'false'   */                                   \
  X(Float) /*   E.g. 1234.5678 or 4.567e3   */                                 \
  X(String) /*   E.g. 'hello' or "hello"   */                                  \
  X(Character) /*   E.g. `a   */                                               \
                                                                               \
  X(None) /*   None   */                                                       \
                                                                               \
  X(LabelDeclaration) /*   @outer   */                                         \
                                                                               \
  /* An identifier. Must start with either a letter or _ (no numbers) */       \
  X(Identifier)

namespace extense {
/*
 * A source token.
 * Represents a syntactic piece of the source code, e.g. a number.
 */
class Token {
public:
  /*
   * Possible token types.
   */
  enum class Type {
#define X(a) a,
    _LIB_EXTENSE__TOKEN__TYPE_ENUM
#undef X
  };

  using Data = std::variant<std::monostate, bool, std::int64_t, double, char,
                            std::string>;

private:
  Source::Location loc;
  std::string_view tokenText;
  Type tokenType;
  Data data_;

public:
  explicit Token(Source::Location location) : loc(std::move(location)) {}
  Token(Source::Location location, std::string_view text, Type type)
      : loc(std::move(location)), tokenText(text), tokenType(type) {}

  void setText(std::string_view text) { tokenText = std::move(text); }
  void setType(Type type) { tokenType = std::move(type); }

  void setData(bool b) { data_ = b; }
  void setData(std::int64_t i) { data_ = i; }
  void setData(double d) { data_ = d; }
  void setData(char c) { data_ = c; }
  void setData(std::string s) { data_ = std::move(s); }
  void clearData() { data_ = std::monostate{}; }

  /*
   * Accessors for the token's location in the source, raw text, and type
   * respectively.
   */

  Source::Location location() const { return loc; }

  std::string_view text() const { return tokenText; }

  Type type() const { return tokenType; }

  Data data() const { return data_; }
};

/*
 * Tokenizes a source stream/string.
 * The source string must not be destructed until the tokens are no longer in
 * use.
 *
 * May throw an LexingError exception if the source could not be
 * tokenized.
 */
std::vector<Token> tokenize(Source &source);
inline std::vector<Token> tokenize(std::string_view source) {
  Source s{source};
  return tokenize(s);
}

/*
 * Exception thrown when tokenize was unable to fetch a token.
 */
class LexingError : public LocatableError {
public:
  LexingError(std::string error, Source::Location loc)
      : LocatableError(std::move(error), std::move(loc)) {}
};

namespace detail {
// Gets the next token in the source file
Token fetchNextToken(Source &source);

/*
 * Tries to match a string of characters with the source.
 * The source's current character is the one after the end of the matched string
 * if the match was successful, and is otherwise not changed.
 */
bool tryMatch(Source &source, std::string_view str);

// Skip all whitespace into the source, so that the current character is the
// beginning of a token
void skipWhitespace(Source &source);

// Lexes different types of tokens. Returns whether they were successful.
bool lexCharacter(Source &source, Token &out);
bool lexString(Source &source, Token &out);
bool lexLabel(Source &source, Token &out);
bool lexNumber(Source &source, Token &out);
bool lexOperator(Source &source, Token &out);
// Also lexes booleans and textual tokens
bool lexIdentifier(Source &source, Token &out);

// Expects the current character when called to be backslash
void lexEscapeSequence(Source &source, char &out);

void lexHexadecimalNumber(Source &source, std::int64_t &out);
void lexOctalNumber(Source &source, std::int64_t &out);
void lexDecimalNumber(Source &source, std::int64_t &out);
void lexBinaryNumber(Source &source, std::int64_t &out);

// EOS = End of Source
inline void throwUnexpectedEOS(Source &source) {
  if (source.currentChar().isValidChar()) return;
  throw LexingError{"Unexpected end of source", source.location()};
}

template <typename Pred>
void skipUntilPermitEOS(Source &source, Pred p) {
  while (!source.currentChar().isAfterSource() &&
         !p(source.currentChar().get()))
    source.nextChar();
}

template <typename Pred>
void skipUntil(Source &source, Pred p) {
  skipUntilPermitEOS(source, p);
  throwUnexpectedEOS(source);
}

template <typename Pred>
void skipPastPermitEOS(Source &source, Pred p) {
  skipUntilPermitEOS(source, p);
  source.nextChar();
}

template <typename Pred>
void skipPast(Source &source, Pred p) {
  skipPastPermitEOS(source, p);
  throwUnexpectedEOS(source);
}
} // namespace detail

std::ostream &operator<<(std::ostream &, const Token &);
std::ostream &operator<<(std::ostream &, Token::Type);
} // namespace extense

#endif // _LIB_EXTENSE__TOKEN_HPP
