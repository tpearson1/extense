/*
  -------------------------------------------------------------------------------
  This file is part of Extense
  -------------------------------------------------------------------------------
  Copyright (c) 2018 Thomas Pearson

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

#ifndef _LIB_EXTENSE__PARSER_HPP
#define _LIB_EXTENSE__PARSER_HPP

#include <iostream>

#include <extense/ast.hpp>
#include <extense/exception.hpp>
#include <extense/token.hpp>

namespace extense {
class ParseError : public LocatableError {
  std::string tokenText_;
  Token::Type tokenType_;

public:
  explicit ParseError(std::string what, const Token &at)
      : LocatableError(std::move(what), at.location()), tokenText_(at.text()),
        tokenType_(at.type()) {}

  const std::string &tokenText() const { return tokenText_; }
  Token::Type tokenType() const { return tokenType_; }
};

namespace detail {
struct BinaryOperatorProperties {
  int precedence;
  bool rightAssociative;
};

constexpr const std::array<BinaryOperatorProperties, 41> binaryOperators = {{
    // { Precedence, RightAssociative }
    {14, false}, // CustomOperator
    {20, false}, // Semicolon
    {20, false}, // Colon

    {1, true}, // Assign
    {1, true}, // PlusEquals
    {1, true}, // MinusEquals
    {1, true}, // MulEquals
    {1, true}, // DivEquals
    {1, true}, // FloorDivEquals
    {1, true}, // PowEquals
    {1, true}, // ModEquals
    {1, true}, // BitAndEquals
    {1, true}, // BitOrEquals
    {1, true}, // BitXorEquals
    {1, true}, // BitLShiftEquals
    {1, true}, // BitRShiftEquals

    {11, false}, // BitAnd
    {9, false}, // BitOr
    {10, false}, // BitXor
    {8, false}, // BitLShift
    {8, false}, // BitRShift
    {3, false}, // And
    {2, false}, // Or
    {5, false}, // Equals
    {5, false}, // NotEquals
    {6, false}, // LessThan
    {6, false}, // LessEquals
    {6, false}, // GreaterThan
    {6, false}, // GreaterEquals
    {19, false}, // Dot
    {20, false}, // SemicolonSemicolon
    {20, false}, // ColonColon
    {7, false}, // Is
    {13, false}, // DotDot
    {14, false}, // Mul
    {14, false}, // FloorDiv
    {15, true}, // Pow
    {14, false}, // Mod
    {14, false}, // Div
    {12, false}, // Plus
    {12, false} // Minus
}};

constexpr const std::array<int, 6> unaryOperators = {{
    // Precedence
    17, // IdentifierName
    17, // Plus
    17, // Minus
    4, // Not
    18, // Exclamation
    17 // BitNot
}};

constexpr int binaryPrecedence(ASTNodeType op) {
  return binaryOperators[static_cast<int>(op)].precedence;
}

constexpr int unaryPrecedence(ASTNodeType op) {
  return unaryOperators[static_cast<int>(op) -
                        static_cast<int>(ASTNodeType::IdentifierName)];
}

constexpr bool rightAssociative(ASTNodeType op) {
  return binaryOperators[static_cast<int>(op)].rightAssociative;
}

constexpr bool isTokenTypeBinaryOperator(Token::Type type) {
  auto typeOrd = static_cast<int>(type);
  return typeOrd >= static_cast<int>(Token::Type::CustomOperator) &&
         typeOrd <= static_cast<int>(Token::Type::Minus);
}

constexpr bool isTokenTypeUnaryOperator(Token::Type type) {
  auto typeOrd = static_cast<int>(type);
  return typeOrd >= static_cast<int>(Token::Type::Div) &&
         typeOrd <= static_cast<int>(Token::Type::BitNot);
}

constexpr ASTNodeType binaryOperatorFromTokenType(Token::Type t) {
  assert(isTokenTypeBinaryOperator(t));
  return static_cast<ASTNodeType>(t);
}

constexpr ASTNodeType unaryOperatorFromTokenType(Token::Type t) {
  assert(isTokenTypeUnaryOperator(t));
  return static_cast<ASTNodeType>(
      static_cast<int>(t) - static_cast<int>(Token::Type::Div) +
      static_cast<int>(ASTNodeType::IdentifierName));
}

constexpr bool terminatesExpression(Token::Type type) {
  auto typeOrd = static_cast<int>(type);
  return typeOrd >= static_cast<int>(Token::Type::EndStatement) &&
         typeOrd <= static_cast<int>(Token::Type::RightBrace);
}

template <typename T>
class ObjectStream {
  const std::vector<T> &objects_;
  int index_ = 0;

public:
  // Stores a reference to the passed vector, and so the vector must outlive the
  // ObjectStream's use
  ObjectStream(const std::vector<T> &objects) : objects_(objects) {}

  const T *at(int index) const {
    if (index < 0 || static_cast<std::size_t>(index) >= objects_.size())
      return nullptr;
    return &objects_[index];
  }

  const T *previous() const { return at(index_ - 1); }
  const T *current() const { return at(index_); }
  const T *peek() const { return at(index_ + 1); }

  int size() const { return objects_.size(); }

  int index() const { return index_; }
  void seek(int index) { index_ = index; }

  void next() { index_++; }

  const std::vector<T> &data() const { return objects_; }
};

template <typename TObjectStream, typename MatchFunc>
bool tryMatch(TObjectStream &s, MatchFunc mf) {
  auto index = s.index();
  if (mf()) return true;

  // If the matching function failed, it should revert back to where it started
  s.seek(index);
  return false;
}

using TokenStream = ObjectStream<Token>;

bool parseLiteral(TokenStream &s, std::unique_ptr<Expr> &out);

template <typename VT, typename TData, extense::Token::Type type>
bool parseLiteralHelper(extense::detail::TokenStream &s,
                        std::unique_ptr<extense::Expr> &out) {
  auto loc = s.current()->location();
  if (s.current()->type() != type) return false;
  out = std::make_unique<extense::ValueExpr>(
      std::move(loc), extense::Value{VT{std::get<TData>(s.current()->data())}});
  s.next();
  return true;
}

bool parseNone(TokenStream &s, std::unique_ptr<Expr> &out);

inline bool parseInt(TokenStream &s, std::unique_ptr<Expr> &out) {
  return parseLiteralHelper<Int, std::int64_t, Token::Type::Integer>(s, out);
}
inline bool parseFloat(TokenStream &s, std::unique_ptr<Expr> &out) {
  return parseLiteralHelper<Float, double, Token::Type::Float>(s, out);
}
inline bool parseBool(TokenStream &s, std::unique_ptr<Expr> &out) {
  return parseLiteralHelper<Bool, bool, Token::Type::Bool>(s, out);
}
inline bool parseChar(TokenStream &s, std::unique_ptr<Expr> &out) {
  return parseLiteralHelper<Char, char, Token::Type::Character>(s, out);
}
inline bool parseString(TokenStream &s, std::unique_ptr<Expr> &out) {
  return parseLiteralHelper<String, std::string, Token::Type::String>(s, out);
}

bool parseScope(TokenStream &s, std::unique_ptr<Expr> &out);

bool parseMap(TokenStream &s, std::unique_ptr<Expr> &out);

// a -> b
ParsedMapping parseMapping(TokenStream &s);

bool parseList(TokenStream &s, std::unique_ptr<Expr> &out);

std::unique_ptr<Expr> parseLabel(TokenStream &s);
std::unique_ptr<Expr> parseExprOrLabel(TokenStream &s);

bool parseParenthesizedCustomOperator(TokenStream &s,
                                      std::unique_ptr<Expr> &out);

bool parseValueExpr(TokenStream &s, std::unique_ptr<Expr> &out);
bool parseIdentifier(TokenStream &s, std::unique_ptr<Expr> &out);

std::unique_ptr<Expr> parsePrimary(TokenStream &s);

std::unique_ptr<Expr> parseUnaryOperator(TokenStream &s);
std::unique_ptr<Expr> parsePrefix(TokenStream &s);
bool parseBinaryOperator(TokenStream &s, std::unique_ptr<Expr> &left, int prec);
bool parseScopeCall(TokenStream &s, std::unique_ptr<Expr> &left, int prec);

bool parseSpecialBinaryOperator(Source::Location loc, ASTNodeType op,
                                std::string_view opText,
                                std::unique_ptr<Expr> &left,
                                std::unique_ptr<Expr> &right);

std::unique_ptr<UnaryOperation>
makeUnaryOperation(Source::Location loc, ASTNodeType op,
                   std::unique_ptr<Expr> operand);

std::unique_ptr<BinaryOperation>
makeBinaryOperation(Source::Location loc, ASTNodeType op,
                    std::unique_ptr<Expr> left, std::unique_ptr<Expr> right);

std::unique_ptr<Expr> parseExprHelper(TokenStream &s, int precedence);
inline std::unique_ptr<Expr> parseExpr(TokenStream &s) {
  return parseExprHelper(s, 0);
}

// Parses the input token stream into an ExprList
std::unique_ptr<ExprList> parse(TokenStream &s);

auto unaryOperationFunc(ASTNodeType type);
auto binaryOperationFunc(ASTNodeType type);

String getIdentifierName(Expr &e);
} // namespace detail

// Parses the input into a single Expr
inline std::unique_ptr<Expr> parseExpr(const std::vector<Token> &s) {
  if (s.empty()) return nullptr;
  detail::TokenStream tokenStream{s};
  return detail::parseExpr(tokenStream);
}

inline std::unique_ptr<Expr> parseExpr(const std::string &s) {
  auto tokens = tokenize(s);
  return parseExpr(tokens);
}

// Parses the input into an ExprList
inline std::unique_ptr<ExprList> parse(const std::vector<Token> &s) {
  detail::TokenStream tokenStream{s};
  return detail::parse(tokenStream);
}

inline std::unique_ptr<ExprList> parse(const std::string &s) {
  auto tokens = tokenize(s);
  return parse(tokens);
}
} // namespace extense

#endif /* _LIB_EXTENSE__PARSER_HPP */
