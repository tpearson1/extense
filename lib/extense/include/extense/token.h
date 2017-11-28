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

#ifndef _LIB_EXTENSE__TOKEN_H
#define _LIB_EXTENSE__TOKEN_H

#include <iosfwd>

#include <extense/source.h>

#define _LIB_EXTENSE__TOKEN__TYPE_ENUM                                         \
  X(Dollar) /*   $   */                                                        \
                                                                               \
  X(Label) /*   @outer   */                                                    \
                                                                               \
  /* Math operators */                                                         \
  X(Plus) /*   +   */                                                          \
  X(Minus) /*   -   */                                                         \
  X(Mul) /*   *   */                                                           \
  X(Div) /*   /   */                                                           \
  X(FloorDiv) /*   //   */                                                     \
  X(Pow) /*   **   */                                                          \
  X(Mod) /*   %   */                                                           \
                                                                               \
  X(CustomOperator) /*   E.g. <=> or |>   */                                   \
                                                                               \
  X(Assign) /*   =   */                                                        \
  X(Dot) /*   .   */                                                           \
  X(DotDot) /*   ..   */                                                       \
  X(Colon) /*   :   */                                                         \
  X(ColonColon) /*   ::   */                                                   \
  X(Exclamation) /* ! */                                                       \
  X(EndStatement) /*   ; or a newline   */                                     \
  X(MapsTo) /* -> */                                                           \
                                                                               \
  X(LeftParen) /*   (   */                                                     \
  X(RightParen) /*   )   */                                                    \
  X(LeftBracket) /*   [   */                                                   \
  X(RightBracket) /*   ]   */                                                  \
  X(LeftBrace) /*   {   */                                                     \
  X(RightBrace) /*   }   */                                                    \
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
  /* Bitwise operators */                                                      \
  X(BitAnd) /*   &   */                                                        \
  X(BitOr) /*   |   */                                                         \
  X(BitXor) /*   ^   */                                                        \
  X(BitNot) /*   ~   */                                                        \
  X(BitLShift) /*   <<   */                                                    \
  X(BitRShift) /*   >>   */                                                    \
                                                                               \
  /* Relative bitwise Operators */                                             \
  X(BitAndEquals) /*   &=   */                                                 \
  X(BitOrEquals) /*   |=   */                                                  \
  X(BitXorEquals) /*   ^=   */                                                 \
  X(BitLShiftEquals) /*   <<=   */                                             \
  X(BitRShiftEquals) /*   >>=   */                                             \
                                                                               \
  /* Comparison operators */                                                   \
  X(Equals) /*   ==   */                                                       \
  X(NotEquals) /*   !=   */                                                    \
  X(LessThan) /*   <   */                                                      \
  X(LessEquals) /*   <=   */                                                   \
  X(GreaterThan) /*   >   */                                                   \
  X(GreaterEquals) /*   >=   */                                                \
                                                                               \
  /* Logical operators */                                                      \
  X(And) /*   and   */                                                         \
  X(Or) /*   or   */                                                           \
  X(Not) /*   not   */                                                         \
                                                                               \
  /* Literals */                                                               \
  X(Integer) /*   E.g. 1234   */                                               \
  X(Bool) /*   Either 'true' or 'false'   */                                   \
  X(Float) /*   E.g. 1234.5678 or 4.567e3   */                                 \
  X(String) /*   E.g. 'hello' or "hello"   */                                  \
  X(Character) /*   E.g. `a   */                                               \
                                                                               \
  /* End of source */                                                          \
  X(EndSource)                                                                 \
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

private:
  Source::Location loc;
  std::string_view tokenText;
  Type tokenType;

public:
  Token(Source::Location location) : loc(std::move(location)) {}
  Token(Source::Location location, std::string_view text, Type type)
      : loc(std::move(location)), tokenText(text), tokenType(type) {}

  void setText(std::string_view text) { tokenText = std::move(text); }
  void setType(Type type) { tokenType = std::move(type); }

  /*
   * Accessors for the token's location in the source, raw text, and type
   * respectively.
   */

  Source::Location location() const {
    assert(tokenType != Type::EndSource);
    return loc;
  }

  std::string_view text() const {
    assert(tokenType != Type::EndSource);
    return tokenText;
  }

  Type type() const { return tokenType; }
};

/*
 * Tokenizes a source stream.
 *
 * May throw an InvalidTokenError exception if the source could not be
 * tokenized.
 */
std::vector<Token> tokenize(Source &source);

/*
 * Exception thrown when tokenize was unable to fetch a token.
 */
class LexingError : public std::runtime_error {
  Source::Location loc;

public:
  LexingError(Source::Location location, const std::string &what)
      : std::runtime_error(what), loc(location) {}

  LexingError(Source::Location location, const char *what)
      : std::runtime_error(what), loc(location) {}

  Source::Location location() const { return loc; }
};
} // namespace extense

std::ostream &operator<<(std::ostream &, const extense::Token &);
std::ostream &operator<<(std::ostream &, extense::Token::Type);

#endif // _LIB_EXTENSE__TOKEN_H
