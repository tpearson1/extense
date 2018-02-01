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

#include <extense/parser.hpp>

#include <catch.hpp>

using namespace extense;
using namespace extense::literals;

TEST_CASE("Querying properties of ASTNodeTypes", "[ASTNodeType]") {
  REQUIRE(detail::binaryPrecedence(ASTNodeType::BitOr) == 9);
  REQUIRE(detail::unaryPrecedence(ASTNodeType::UnaryMinus) == 17);
  REQUIRE(detail::rightAssociative(ASTNodeType::Pow));
}

TEST_CASE("Categorizing and converting Token::Types", "[Token::Type]") {
  REQUIRE(detail::isTokenTypeBinaryOperator(Token::Type::Plus));
  REQUIRE(detail::isTokenTypeBinaryOperator(Token::Type::BitAnd));
  REQUIRE(detail::isTokenTypeBinaryOperator(Token::Type::Assign));
  REQUIRE(detail::isTokenTypeBinaryOperator(Token::Type::CustomOperator));
  REQUIRE(!detail::isTokenTypeBinaryOperator(Token::Type::Not));

  REQUIRE(detail::isTokenTypeUnaryOperator(Token::Type::Plus));
  REQUIRE(detail::isTokenTypeUnaryOperator(Token::Type::BitNot));
  REQUIRE(!detail::isTokenTypeUnaryOperator(Token::Type::CustomOperator));

  REQUIRE(detail::binaryOperatorFromTokenType(Token::Type::And) ==
          ASTNodeType::And);
  REQUIRE(detail::binaryOperatorFromTokenType(Token::Type::Assign) ==
          ASTNodeType::Assign);

  REQUIRE(detail::unaryOperatorFromTokenType(Token::Type::Minus) ==
          ASTNodeType::UnaryMinus);
  REQUIRE(detail::unaryOperatorFromTokenType(Token::Type::Not) ==
          ASTNodeType::Not);

  REQUIRE(detail::terminatesExpression(Token::Type::RightParen));
  REQUIRE(detail::terminatesExpression(Token::Type::RightBracket));
  REQUIRE(detail::terminatesExpression(Token::Type::RightBrace));
  REQUIRE(detail::terminatesExpression(Token::Type::EndStatement));
  REQUIRE(detail::terminatesExpression(Token::Type::EndSource));
  REQUIRE(!detail::terminatesExpression(Token::Type::Plus));
}

TEST_CASE("Using ObjectStream", "[ObjectStream, TokenStream]") {
  std::vector objects = {1, 3, 4, 17, 29};
  auto s = detail::ObjectStream<int>{objects};
  REQUIRE(s.data() == objects);

  REQUIRE(*s.at(0) == 1);
  REQUIRE(*s.at(2) == 4);
  REQUIRE(s.at(5) == nullptr);

  REQUIRE(s.index() == 0);
  REQUIRE(s.previous() == nullptr);
  REQUIRE(*s.current() == 1);
  REQUIRE(*s.peek() == 3);

  s.next();
  REQUIRE(s.index() == 1);
  REQUIRE(*s.previous() == 1);
  REQUIRE(*s.current() == 3);
  REQUIRE(*s.peek() == 4);

  s.seek(4);
  REQUIRE(s.index() == 4);
  REQUIRE(*s.previous() == 17);
  REQUIRE(*s.current() == 29);
  REQUIRE(s.peek() == nullptr);

  s.seek(17);
  REQUIRE(s.index() == 17);
  REQUIRE(s.previous() == nullptr);
  REQUIRE(s.current() == nullptr);
  REQUIRE(s.peek() == nullptr);
}

TEST_CASE("Detail parsing functions", "[parse]") {
  SECTION("tryMatch") {
    auto tokens = tokenize("())[");
    detail::TokenStream s{tokens};

    auto result = detail::tryMatch(s, [&]() {
      s.seek(2);
      return true;
    });
    REQUIRE(result);
    REQUIRE(s.index() == 2);

    result = detail::tryMatch(s, [&]() {
      s.seek(3);
      return false;
    });
    REQUIRE(!result);
    REQUIRE(s.index() == 2);
  }

  SECTION("parseLiteral and literal functions") {
    auto checkLiteralParse = [](auto str, auto parseFunc,
                                const auto &expectedVal) {
      auto token = tokenize(str);
      detail::TokenStream s{token};
      std::unique_ptr<Expr> result;

      REQUIRE(parseFunc(s, result));
      REQUIRE(result->type() == ASTNodeType::ValueExpr);

      auto value = static_cast<ValueExpr *>(result.get())->value();
      using ExpectedValueType = std::decay_t<decltype(expectedVal)>;
      REQUIRE(value.is<ExpectedValueType>());
      REQUIRE(get<ExpectedValueType>(value) == expectedVal);
    };

    checkLiteralParse("None", detail::parseNone, none);

    checkLiteralParse("742", detail::parseInt, 742_ei);

    auto token = tokenize("7.34e2");
    detail::TokenStream s{token};
    std::unique_ptr<Expr> result;
    REQUIRE(detail::parseFloat(s, result));
    REQUIRE(result->type() == ASTNodeType::ValueExpr);
    auto value = static_cast<ValueExpr *>(result.get())->value();
    REQUIRE(value.is<Float>());
    REQUIRE(get<Float>(value).value == Approx(734.0));

    checkLiteralParse("true", detail::parseBool, Bool::t);
    checkLiteralParse("false", detail::parseBool, Bool::f);

    checkLiteralParse("`a", detail::parseChar, 'a'_ec);
    checkLiteralParse("`\\x32", detail::parseChar, '\x32'_ec);

    checkLiteralParse("'Hello, World'", detail::parseString, "Hello, World"_es);

    checkLiteralParse("true", detail::parseLiteral, Bool::t);
    checkLiteralParse("None", detail::parseLiteral, none);
    checkLiteralParse("34", detail::parseLiteral, 34_ei);
  }

  SECTION("parseScope") {
    auto tokens = tokenize("[7,3]");
    detail::TokenStream s{tokens};
    std::unique_ptr<Expr> result;
    REQUIRE(detail::parseScope(s, result));
    REQUIRE(result->type() == ASTNodeType::ExprList);

    std::ostringstream os;
    result->dump(os);
    REQUIRE(os.str() == R"(ExprList: Expressions below
  ValueExpr: 7
  ValueExpr: 3
)");
  }

  SECTION("parseMap and parseMapping") {
    auto tokens = tokenize("{7 -> 6, 5 -> 4}");
    detail::TokenStream s{tokens};
    std::unique_ptr<Expr> result;
    REQUIRE(detail::parseMap(s, result));
    REQUIRE(result->type() == ASTNodeType::MapConstructor);

    std::ostringstream os;
    result->dump(os);
    REQUIRE(os.str() == R"(MapConstructor: Mappings below
  Mapping
    ValueExpr: 7
    ValueExpr: 6
  Mapping
    ValueExpr: 5
    ValueExpr: 4
)");

    s.seek(1);
    auto mapping = detail::parseMapping(s);
    REQUIRE(mapping.key->type() == ASTNodeType::ValueExpr);
    REQUIRE(mapping.value->type() == ASTNodeType::ValueExpr);

    auto value = static_cast<ValueExpr *>(mapping.key.get())->value();
    REQUIRE(value.is<Int>());
    REQUIRE(get<Int>(value).value == 7);

    value = static_cast<ValueExpr *>(mapping.value.get())->value();
    REQUIRE(value.is<Int>());
    REQUIRE(get<Int>(value).value == 6);
  }

  SECTION("parseList") {
    auto tokens = tokenize("(3, 4, 5)");
    detail::TokenStream s{tokens};
    std::unique_ptr<Expr> result;
    REQUIRE(detail::parseList(s, result));
    REQUIRE(result->type() == ASTNodeType::ListConstructor);

    std::ostringstream os;
    result->dump(os);
    REQUIRE(os.str() == R"(ListConstructor: Elements below
  ValueExpr: 3
  ValueExpr: 4
  ValueExpr: 5
)");

    // Parenthesized expression
    tokens = tokenize("(3)");
    s.seek(0);
    REQUIRE(detail::parseList(s, result));
    REQUIRE(result->type() == ASTNodeType::ValueExpr);

    std::ostringstream os2;
    result->dump(os2);
    REQUIRE(os2.str() == "ValueExpr: 3\n");
  }

  SECTION("parseLabel") {
    auto token = tokenize("@thisisalabel");
    detail::TokenStream s{token};
    auto result = detail::parseLabel(s);
    REQUIRE(result);
    REQUIRE(result->type() == ASTNodeType::Label);
    REQUIRE(static_cast<Label *>(result.get())->name() == "thisisalabel");
  }

  SECTION("parseIdentifier") {
    auto token = tokenize("thisisanidentifier");
    detail::TokenStream s{token};
    std::unique_ptr<Expr> result;
    REQUIRE(detail::parseIdentifier(s, result));
    REQUIRE(result->type() == ASTNodeType::Identifier);
    REQUIRE(static_cast<Identifier *>(result.get())->name() ==
            "thisisanidentifier");

    token = tokenize("(+&*)");
    s.seek(0);
    REQUIRE(detail::parseIdentifier(s, result));
    REQUIRE(result->type() == ASTNodeType::Identifier);
    REQUIRE(static_cast<Identifier *>(result.get())->name() == "+&*");
  }

  SECTION("parseUnaryOperator") {
    auto tokens = tokenize("-43");
    detail::TokenStream s{tokens};
    auto result = detail::parseUnaryOperator(s);
    REQUIRE(result);
    REQUIRE(result->type() == ASTNodeType::UnaryMinus);

    std::ostringstream os;
    result->dump(os);
    REQUIRE(os.str() == R"(UnaryOperation: type 'UnaryMinus'
  ValueExpr: 43
)");
  }

  SECTION("parseBinaryOperator") {
    auto tokens = tokenize("* 43");
    detail::TokenStream s{tokens};

    std::unique_ptr<Expr> v = std::make_unique<ValueExpr>(Value{17_ei});
    REQUIRE(detail::parseBinaryOperator(s, v, 0));
    REQUIRE(v->type() == ASTNodeType::Mul);

    std::ostringstream os;
    v->dump(os);
    REQUIRE(os.str() == R"(BinaryOperation: type 'Mul'
  ValueExpr: 17
  ValueExpr: 43
)");
  }

  SECTION("parseScopeCall") {
    // Although integers cannot be called, it simplifies testing
    auto argument = tokenize("43");
    detail::TokenStream s{argument};

    std::unique_ptr<Expr> v = std::make_unique<ValueExpr>(Value{42_ei});
    REQUIRE(detail::parseScopeCall(s, v, 0));
    REQUIRE(v->type() == ASTNodeType::ScopeCall);

    std::ostringstream os;
    v->dump(os);
    REQUIRE(os.str() == R"(ScopeCall: Scope and argument below
  ValueExpr: 42
  ValueExpr: 43
)");
  }
}

TEST_CASE("Parsing functions", "[parseExpr, parse]") {
  SECTION("parseExpr") {
    auto expr = parseExpr("3 + (x 4 * 2) / 3");
    REQUIRE(expr);

    std::ostringstream os;
    expr->dump(os);
    REQUIRE(os.str() == R"(BinaryOperation: type 'Plus'
  ValueExpr: 3
  BinaryOperation: type 'Div'
    BinaryOperation: type 'Mul'
      ScopeCall: Scope and argument below
        Identifier: name 'x'
        ValueExpr: 4
      ValueExpr: 2
    ValueExpr: 3
)");

    bool threw = false;
    try {
      parseExpr("3 *");
    } catch (const ParseError &) { threw = true; }
    REQUIRE(threw);
  }

  SECTION("parse") {
    auto exprList = parse("-q, 3 + 4, a - b, c *= d, @xyz");
    REQUIRE(exprList);

    std::ostringstream os;
    exprList->dump(os);
    REQUIRE(os.str() == R"(ExprList: Expressions below
  UnaryOperation: type 'UnaryMinus'
    Identifier: name 'q'
  BinaryOperation: type 'Plus'
    ValueExpr: 3
    ValueExpr: 4
  BinaryOperation: type 'Minus'
    Identifier: name 'a'
    Identifier: name 'b'
  BinaryOperation: type 'MulEquals'
    Identifier: name 'c'
    Identifier: name 'd'
  Label: name 'xyz'
)");

    auto empty = parse("");
    REQUIRE(empty);
    std::ostringstream os2;
    empty->dump(os2);
    REQUIRE(os2.str() == R"(ExprList: Expressions below
  ValueExpr: None
)");
  }
}