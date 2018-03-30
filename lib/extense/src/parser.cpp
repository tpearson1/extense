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

bool extense::detail::parseLiteral(TokenStream &s, std::unique_ptr<Expr> &out) {
  return parseBool(s, out) || parseInt(s, out) || parseFloat(s, out) ||
         parseString(s, out) || parseChar(s, out) || parseNone(s, out);
}

static bool parseToken(extense::detail::TokenStream &s,
                       extense::Token::Type type) {
  if (s.current()->type() != type) return false;
  s.next();
  return true;
}

static void expectToken(extense::detail::TokenStream &s) {
  if (s.current()) return;
  throw extense::ParseError{"Unexpected end of source",
                            s.data()[s.index() - 1]};
}

static void consume(extense::detail::TokenStream &s,
                    extense::Token::Type type) {
  if (parseToken(s, type)) return;
  std::ostringstream os;
  os << "Expected " << type;
  throw extense::ParseError{os.str(), *s.current()};
}

bool extense::detail::parseNone(TokenStream &s, std::unique_ptr<Expr> &out) {
  auto loc = s.current()->location();
  if (parseToken(s, Token::Type::None)) {
    out = std::make_unique<ValueExpr>(std::move(loc), noneValue);
    return true;
  }
  return false;
}

// Always returns true as it always succeeds, and returning a bool allows it to
// integrate nicely with other parsing functions
static bool skipEndStatement(extense::detail::TokenStream &s) {
  parseToken(s, extense::Token::Type::EndStatement);
  return true;
}

std::unique_ptr<extense::Expr> extense::detail::parseLabel(TokenStream &s) {
  auto loc = s.current()->location();
  if (s.current()->type() != Token::Type::LabelDeclaration) return nullptr;
  auto fullText = s.current()->text();
  auto withoutAtSymbol = std::string(fullText.begin() + 1, fullText.end());
  s.next();
  return std::make_unique<LabelDeclaration>(std::move(loc), withoutAtSymbol);
}

std::unique_ptr<extense::Expr>
extense::detail::parseExprOrLabel(TokenStream &s) {
  auto out = parseLabel(s);
  if (out) return out;
  out = parseExpr(s);
  return out;
}

// Always returns true
template <typename StatementType, typename ParseFunc>
static std::vector<StatementType>
parseExpressionList(extense::detail::TokenStream &s,
                    extense::Token::Type terminator, ParseFunc pf) {
  std::vector<StatementType> out;
  while (true) {
    if (s.current()->type() == terminator) {
      s.next();
      return out;
    }
    StatementType st = pf(s);
    out.push_back(std::move(st));

    expectToken(s);
    skipEndStatement(s);
  }
}

bool extense::detail::parseScope(TokenStream &s, std::unique_ptr<Expr> &out) {
  auto loc = s.current()->location();

  // Left bracket
  if (!parseToken(s, Token::Type::LeftBracket)) return false;
  expectToken(s);
  skipEndStatement(s);
  // List of expressions and right bracket
  expectToken(s);
  auto exprs = parseExpressionList<std::unique_ptr<Expr>>(
      s, Token::Type::RightBracket, parseExprOrLabel);

  out = std::make_unique<ExprList>(std::move(loc), std::move(exprs));
  return true;
}

bool extense::detail::parseMap(TokenStream &s, std::unique_ptr<Expr> &out) {
  auto loc = s.current()->location();

  // Left brace
  if (!parseToken(s, Token::Type::LeftBrace)) return false;
  expectToken(s);
  skipEndStatement(s);
  // List of mappings and right brace
  expectToken(s);
  auto mappings = parseExpressionList<ParsedMapping>(s, Token::Type::RightBrace,
                                                     parseMapping);

  out = std::make_unique<MapConstructor>(std::move(loc), std::move(mappings));
  return true;
}

extense::ParsedMapping extense::detail::parseMapping(TokenStream &s) {
  ParsedMapping out;
  // Parse the key
  out.key = parseExpr(s);
  // The MapsTo token
  expectToken(s);
  parseToken(s, Token::Type::MapsTo);
  // And the value
  expectToken(s);
  out.value = parseExpr(s);
  return out;
}

bool extense::detail::parseList(TokenStream &s, std::unique_ptr<Expr> &out) {
  auto loc = s.current()->location();
  return tryMatch(s, [&]() {
    // If there is not an EndStatement in the parentheses, then it is not a List
    // and is instead used for operator precedence purposes
    bool hasEndStatement = false;

    auto checkEndStatement = [&]() {
      if (!parseToken(s, Token::Type::EndStatement)) return;
      hasEndStatement = true;
      expectToken(s);
    };

    if (!parseToken(s, Token::Type::LeftParen)) return false;
    expectToken(s);
    checkEndStatement();

    std::vector<std::unique_ptr<Expr>> elements;
    if (parseToken(s, Token::Type::RightParen)) {
      out = std::make_unique<ListConstructor>(std::move(loc),
                                              std::move(elements));
      return true;
    }

    elements.push_back(parseExpr(s));
    expectToken(s);

    checkEndStatement();
    if (!hasEndStatement) {
      // We have parsed a LeftParen and an expression, and since there is no end
      // statement, for parsing to be successful, it must just be a
      // parenthesized expression, so there should now be a RightParen.
      consume(s, Token::Type::RightParen);
      out = std::move(elements[0]);
      return true;
    }

    while (s.current() && s.current()->type() != Token::Type::RightParen) {
      elements.push_back(parseExpr(s));
      expectToken(s);
      skipEndStatement(s);
    }

    s.next();
    out =
        std::make_unique<ListConstructor>(std::move(loc), std::move(elements));
    return true;
  });
}

bool extense::detail::parseParenthesizedCustomOperator(
    TokenStream &s, std::unique_ptr<Expr> &out) {
  return tryMatch(s, [&]() {
    auto loc = s.current()->location();

    if (!parseToken(s, Token::Type::LeftParen)) return false;
    expectToken(s);
    if (s.current()->type() != Token::Type::CustomOperator) return false;
    auto text = s.current()->text();
    s.next();
    expectToken(s);
    if (!parseToken(s, Token::Type::RightParen)) return false;
    out = std::make_unique<Identifier>(std::move(loc), std::string(text));
    return true;
  });
}

bool extense::detail::parseValueExpr(TokenStream &s,
                                     std::unique_ptr<Expr> &out) {
  return parseLiteral(s, out) || parseScope(s, out) || parseList(s, out) ||
         parseMap(s, out);
}

bool extense::detail::parseIdentifier(TokenStream &s,
                                      std::unique_ptr<Expr> &out) {
  if (s.current()->type() != Token::Type::Identifier)
    return parseParenthesizedCustomOperator(s, out);
  out = std::make_unique<Identifier>(s.current()->location(),
                                     std::string(s.current()->text()));
  s.next();
  return true;
}

std::unique_ptr<extense::Expr> extense::detail::parsePrimary(TokenStream &s) {
  std::unique_ptr<Expr> out;
  parseIdentifier(s, out) || parseValueExpr(s, out);
  return out;
}

std::unique_ptr<extense::Expr>
extense::detail::parseUnaryOperator(TokenStream &s) {
  auto &opToken = *s.current();
  auto opLoc = opToken.location();
  auto opTokenType = opToken.type();
  if (!isTokenTypeUnaryOperator(opTokenType)) return nullptr;

  auto op = unaryOperatorFromTokenType(opTokenType);
  auto opPrecedence = unaryPrecedence(op);

  s.next();
  if (!s.current())
    throw ParseError{"Expected expression after unary operator", opToken};

  auto operand = parseExprHelper(s, opPrecedence);
  return makeUnaryOperation(opLoc, op, std::move(operand));
}

std::unique_ptr<extense::Expr> extense::detail::parsePrefix(TokenStream &s) {
  // Try parse a primary
  auto out = parsePrimary(s);
  if (out) return out;

  // That failed, try parse a UnaryOperator
  out = parseUnaryOperator(s);
  if (out) return out;

  throw ParseError{"Unable to parse expression", *s.current()};
}

bool extense::detail::parseBinaryOperator(TokenStream &s,
                                          std::unique_ptr<Expr> &left,
                                          int prec) {
  auto &opToken = *s.current();
  auto opLoc = opToken.location();
  auto opTokenType = opToken.type();
  if (!isTokenTypeBinaryOperator(opTokenType)) return false;

  auto op = binaryOperatorFromTokenType(opTokenType);
  auto opPrecedence = binaryPrecedence(op);
  if (prec >= opPrecedence) return false;

  s.next();
  if (!s.current())
    throw ParseError{"Expected expression after binary operator", opToken};

  auto right = parseExprHelper(s, rightAssociative(op) ? (opPrecedence - 1) :
                                                         opPrecedence);

  if (parseSpecialBinaryOperator(opLoc, op, opToken.text(), left, right))
    return true;

  left = makeBinaryOperation(std::move(opLoc), op, std::move(left),
                             std::move(right));
  return true;
}

bool extense::detail::parseScopeCall(TokenStream &s,
                                     std::unique_ptr<Expr> &left, int prec) {
  auto loc = s.current()->location();

  constexpr int functionCallPrecedence = 16;
  if (isTokenTypeBinaryOperator(s.current()->type()) ||
      prec >= functionCallPrecedence)
    return false;

  // Try parse calling a scope, and return.
  // One is subtracted from parseExpr's precedence argument, for
  // right-associativity.
  auto argument = parseExprHelper(s, functionCallPrecedence - 1);
  left = std::make_unique<ScopeCall>(std::move(loc), std::move(left),
                                     std::move(argument));
  return true;
}

std::unique_ptr<extense::Expr> extense::detail::parseExprHelper(TokenStream &s,
                                                                int prec) {
  auto left = parsePrefix(s);

  while (s.current() && !terminatesExpression(s.current()->type())) {
    auto success = parseBinaryOperator(s, left, prec);
    if (success) continue;
    success = parseScopeCall(s, left, prec);
    if (!success) break; // Neither were successful
  }

  return left;
}

std::unique_ptr<extense::ExprList> extense::detail::parse(TokenStream &s) {
  auto loc = s.current()->location();

  std::vector<std::unique_ptr<Expr>> exprs;
  if (!s.current()) {
    // An empty TokenStream is still forms a valid (but empty) AST
    return std::make_unique<ExprList>(loc, std::move(exprs));
  }

  // Parsing is considered a failure if the whole input is not parsed
  skipEndStatement(s);
  expectToken(s);
  exprs = parseExpressionList<std::unique_ptr<Expr>>(s, Token::Type::EndSource,
                                                     parseExprOrLabel);

  // On failure we do not need to reset the TokenStream's index as a
  // TokenStream will have been created just for this function
  if (s.current() && s.current()->type() == Token::Type::EndSource)
    throw ParseError{"Unable to parse the entire input", *s.current()};

  // Success
  return std::make_unique<ExprList>(std::move(loc), std::move(exprs));
}

extense::String extense::detail::getIdentifierName(Expr &e) {
  if (e.type() != ASTNodeType::Identifier)
    throw InvalidUnaryOperation{"<Not identifier>", "Expected identifier"};
  return String{static_cast<const Identifier &>(e).name()};
}

auto extense::detail::unaryOperationFunc(extense::ASTNodeType type) {
  assert(isUnaryOperator(type));

  constexpr std::array<extense::UnaryOperation::Function *, 6>
      unaryOperationFuncs = {
          {// IdentifierName
           [](auto &, auto &e) { return Value{getIdentifierName(e)}; },
           // UnaryPlus
           [](auto &s, auto &e) { return ops::add(constEval(s, e)); },
           // UnaryMinus
           [](auto &s, auto &e) { return ops::sub(constEval(s, e)); },
           // LogicalNot
           [](auto &s, auto &e) { return ops::logicalNot(constEval(s, e)); },
           // Exclamation
           [](auto &s, auto &e) {
             return mutableEval(
                 s, e, [](auto &mutE) { return Value{ops::ref(mutE)}; });
           },
           // BitNot
           [](auto &s, auto &e) { return ops::bitNot(constEval(s, e)); }}};

  auto index = static_cast<int>(type) -
               static_cast<int>(extense::ASTNodeType::IdentifierName);
  return unaryOperationFuncs[index];
}

static extense::Value reflexiveIndexCall(extense::Value a, extense::Value v) {
  auto scopeElement = extense::ops::index(a, v);
  if (!scopeElement.is<extense::Scope>()) {
    throw extense::InvalidBinaryOperation{
        a, v, "Expected scope as result of indexing lhs"};
  }

  return extense::get<extense::Scope>(scopeElement)(a);
}

auto extense::detail::binaryOperationFunc(ASTNodeType type) {
  assert(isBinaryOperator(type));

  constexpr std::array<BinaryOperation::Function *, 40> binaryOperationFuncs = {
      {// Assign
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](Value &mutA) {
           Value rhs = constEval(s, b);
           if (mutA.is<Reference>() && !rhs.is<Reference>())
             *get<Reference>(mutA) = rhs.flatten();
           else
             mutA = rhs;
           return noneValue;
         });
       },
       // PlusEquals
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](auto &mutA) {
           return ops::addEquals(mutA, constEval(s, b));
         });
       },
       // MinusEquals
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](auto &mutA) {
           return ops::subEquals(mutA, constEval(s, b));
         });
       },
       // MulEquals
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](auto &mutA) {
           return ops::mulEquals(mutA, constEval(s, b));
         });
       },
       // DivEquals
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](auto &mutA) {
           return ops::divEquals(mutA, constEval(s, b));
         });
       },
       // FloorDivEquals
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](auto &mutA) {
           return ops::floorDivEquals(mutA, constEval(s, b));
         });
       },
       // PowEquals
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](auto &mutA) {
           return ops::powEquals(mutA, constEval(s, b));
         });
       },
       // ModEquals
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](auto &mutA) {
           return ops::modEquals(mutA, constEval(s, b));
         });
       },
       // BitAndEquals
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](auto &mutA) {
           return ops::bitAndEquals(mutA, constEval(s, b));
         });
       },
       // BitOrEquals
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](auto &mutA) {
           return ops::bitOrEquals(mutA, constEval(s, b));
         });
       },
       // BitXorEquals
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](auto &mutA) {
           return ops::bitXorEquals(mutA, constEval(s, b));
         });
       },
       // BitLShiftEquals
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](auto &mutA) {
           return ops::bitLShiftEquals(mutA, constEval(s, b));
         });
       },
       // BitRShiftEquals
       [](auto &s, auto &a, auto &b) {
         return mutableEval(s, a, [&s, &b](auto &mutA) {
           return ops::bitRShiftEquals(mutA, constEval(s, b));
         });
       },

       // BitAnd
       [](auto &s, auto &a, auto &b) {
         return ops::bitAnd(constEval(s, a), constEval(s, b));
       },
       // BitOr
       [](auto &s, auto &a, auto &b) {
         return ops::bitOr(constEval(s, a), constEval(s, b));
       },
       // BitXor
       [](auto &s, auto &a, auto &b) {
         return ops::bitXor(constEval(s, a), constEval(s, b));
       },
       // BitLShift
       [](auto &s, auto &a, auto &b) {
         return ops::bitLShift(constEval(s, a), constEval(s, b));
       },
       // BitRShift
       [](auto &s, auto &a, auto &b) {
         return ops::bitRShift(constEval(s, a), constEval(s, b));
       },
       // And
       [](auto &s, auto &a, auto &b) {
         return ops::logicalAnd(constEval(s, a), constEval(s, b));
       },
       // Or
       [](auto &s, auto &a, auto &b) {
         return ops::logicalOr(constEval(s, a), constEval(s, b));
       },
       // Equals
       [](auto &s, auto &a, auto &b) {
         return Value{ops::equal(constEval(s, a), constEval(s, b))};
       },
       // NotEquals
       [](auto &s, auto &a, auto &b) {
         return Value{ops::notEqual(constEval(s, a), constEval(s, b))};
       },
       // LessThan
       [](auto &s, auto &a, auto &b) {
         return ops::lessThan(constEval(s, a), constEval(s, b));
       },
       // LessEquals
       [](auto &s, auto &a, auto &b) {
         return ops::lessEquals(constEval(s, a), constEval(s, b));
       },
       // GreaterThan
       [](auto &s, auto &a, auto &b) {
         return ops::greaterThan(constEval(s, a), constEval(s, b));
       },
       // GreaterEquals
       [](auto &s, auto &a, auto &b) {
         return ops::greaterEquals(constEval(s, a), constEval(s, b));
       },
       // Dot
       [](auto &s, auto &a, auto &b) {
         Value bEvaled = constEval(s, b);
         if (!bEvaled.is<Scope>()) {
           throw InvalidBinaryOperation{
               "<Unknown type>", bEvaled.typeAsString(),
               "Expected scope to call with '.' operator"};
         }

         return get<Scope>(bEvaled)(constEval(s, a));
       },
       // SemicolonSemicolon
       [](auto &s, auto &a, auto &b) {
         return reflexiveIndexCall(constEval(s, a),
                                   Value{getIdentifierName(b)});
       },
       // ColonColon
       [](auto &s, auto &a, auto &b) {
         return reflexiveIndexCall(constEval(s, a), constEval(s, b));
       },
       // Is
       [](auto &s, auto &a, auto &t) {
         Value tStr = constEval(s, t);
         if (!tStr.is<String>()) throw InvalidUnaryOperation{tStr};
         return Value{ops::is(constEval(s, a), get<String>(tStr).value)};
       },
       // DotDot
       [](auto &s, auto &a, auto &b) {
         return ops::dotDot(constEval(s, a), constEval(s, b));
       },
       // Semicolon
       [](auto &s, auto &a, auto &b) {
         return ops::index(constEval(s, a), Value{getIdentifierName(b)});
       },
       // Colon
       [](auto &s, auto &a, auto &b) {
         return ops::index(constEval(s, a), constEval(s, b));
       },
       // Mul
       [](auto &s, auto &a, auto &b) {
         return ops::mul(constEval(s, a), constEval(s, b));
       },
       // FloorDiv
       [](auto &s, auto &a, auto &b) {
         return ops::floorDiv(constEval(s, a), constEval(s, b));
       },
       // Pow
       [](auto &s, auto &a, auto &b) {
         return ops::pow(constEval(s, a), constEval(s, b));
       },
       // Mod
       [](auto &s, auto &a, auto &b) {
         return ops::mod(constEval(s, a), constEval(s, b));
       },
       // Div
       [](auto &s, auto &a, auto &b) {
         return ops::div(constEval(s, a), constEval(s, b));
       },
       // Plus
       [](auto &s, auto &a, auto &b) {
         return ops::add(constEval(s, a), constEval(s, b));
       },
       // Minus
       [](auto &s, auto &a, auto &b) {
         return ops::sub(constEval(s, a), constEval(s, b));
       }}};
  return binaryOperationFuncs[static_cast<int>(type) - 1];
}

bool extense::detail::parseSpecialBinaryOperator(Source::Location loc,
                                                 ASTNodeType op,
                                                 std::string_view opText,
                                                 std::unique_ptr<Expr> &left,
                                                 std::unique_ptr<Expr> &right) {
  if (op == ASTNodeType::CustomOperator) {
    left = std::make_unique<CustomOperation>(
        std::move(loc), std::string(opText), std::move(left), std::move(right));
    return true;
  }

  if (op == ASTNodeType::Colon) {
    left = std::make_unique<CharMutableBinaryOperation>(
        std::move(loc), op, binaryOperationFunc(op),
        [](auto &s, auto &a, auto &i) {
          auto iEvaled = constEval(s, i);
          return mutableEval(s, a, [&iEvaled](Value &aEvaled) -> Value * {
            // This is handled separately (tryCharMutableEval)
            if (aEvaled.is<String>()) return nullptr;

            return &ops::mutableIndex(aEvaled, iEvaled);
          });
        },
        [](auto &s, auto &a, auto &i) {
          Value iEvaled = constEval(s, i);
          return mutableEval(s, a, [&iEvaled](Value &aEvaled) -> Char * {
            // Can only get a mutable character from indexing a String with an
            // Int
            if (!aEvaled.is<String>() || !iEvaled.is<Int>()) return nullptr;

            return &get<String>(aEvaled)[get<Int>(iEvaled)];
          });
        },
        std::move(left), std::move(right));
    return true;
  }

  if (op == ASTNodeType::Semicolon) {
    left = std::make_unique<MutableBinaryOperation>(
        std::move(loc), op, binaryOperationFunc(op),
        [](auto &s, auto &a, auto &i) {
          return mutableEval(s, a, [&](auto &aEvaled) {
            return &ops::mutableIndex(aEvaled, Value{getIdentifierName(i)});
          });
        },
        std::move(left), std::move(right));
    return true;
  }

  return false;
}

std::unique_ptr<extense::UnaryOperation>
extense::detail::makeUnaryOperation(Source::Location loc, ASTNodeType op,
                                    std::unique_ptr<Expr> operand) {
  return std::make_unique<UnaryOperation>(
      std::move(loc), op, unaryOperationFunc(op), std::move(operand));
}

std::unique_ptr<extense::BinaryOperation>
extense::detail::makeBinaryOperation(Source::Location loc, ASTNodeType op,
                                     std::unique_ptr<Expr> left,
                                     std::unique_ptr<Expr> right) {
  return std::make_unique<BinaryOperation>(std::move(loc), op,
                                           binaryOperationFunc(op),
                                           std::move(left), std::move(right));
}
