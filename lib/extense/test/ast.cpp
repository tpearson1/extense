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

#include <extense/ast.hpp>

#include <catch.hpp>

using namespace extense;
using namespace extense::literals;

TEST_CASE("Classifying and printing AST node types", "[ASTNodeType]") {
  SECTION("isUnaryOperator") {
    REQUIRE(isUnaryOperator(ASTNodeType::UnaryPlus));
    REQUIRE(isUnaryOperator(ASTNodeType::Not));
    REQUIRE(isUnaryOperator(ASTNodeType::BitNot));
    REQUIRE(!isUnaryOperator(ASTNodeType::Plus));
    REQUIRE(!isUnaryOperator(ASTNodeType::ExprList));
  }

  SECTION("isBinaryOperator") {
    REQUIRE(isBinaryOperator(ASTNodeType::Assign));
    REQUIRE(isBinaryOperator(ASTNodeType::Equals));
    REQUIRE(isBinaryOperator(ASTNodeType::Minus));
    REQUIRE(!isBinaryOperator(ASTNodeType::UnaryPlus));
    REQUIRE(!isBinaryOperator(ASTNodeType::ScopeCall));
  }

  SECTION("Printing") {
    // If these two work, then the others should also
    std::ostringstream os;
    os << ASTNodeType::PowEquals;
    REQUIRE(os.str() == "PowEquals");

    std::ostringstream os2;
    os2 << ASTNodeType::Exclamation;
    REQUIRE(os2.str() == "Exclamation");
  }
}

TEST_CASE("Manipulating and dumping expressions", "[Expr]") {
  std::ostringstream indent;
  makeIndent(indent, 6);
  REQUIRE(indent.str() == "      ");
  makeIndent(indent, 0);
  REQUIRE(indent.str() == "      ");
  makeIndent(indent, -5);
  REQUIRE(indent.str() == "      ");

  SECTION("ValueExpr") {
    auto e = ValueExpr{Value{"Hello"_es}};
    REQUIRE(e.type() == ASTNodeType::ValueExpr);
    REQUIRE(e.value().is<String>());
    REQUIRE(get<String>(e.value()).value == "Hello");
    std::ostringstream dump;
    e.dump(dump);
    REQUIRE(dump.str() == "ValueExpr: \"Hello\"\n");

    auto e2 = ValueExpr{Value{17_ei}};
    REQUIRE(e2.type() == ASTNodeType::ValueExpr);
    REQUIRE(e2.value().is<Int>());
    REQUIRE(get<Int>(e2.value()).value == 17);
    std::ostringstream dump2;
    e2.dump(dump2);
    REQUIRE(dump2.str() == "ValueExpr: 17\n");
  }

  // REVIEW: Evaluation once labels are properly implemented
  SECTION("Label") {
    auto label = Label{"outer"};
    REQUIRE(label.type() == ASTNodeType::Label);
    REQUIRE(label.name() == "outer");
    label.rename("inner");
    REQUIRE(label.name() == "inner");

    std::ostringstream dump;
    label.dump(dump);
    REQUIRE(dump.str() == "Label: name 'inner'\n");
  }

  // REVIEW: Evaluation once identifiers are properly implemented
  SECTION("Identifier") {
    auto ident = Identifier{"sum"};
    REQUIRE(ident.type() == ASTNodeType::Identifier);
    REQUIRE(ident.name() == "sum");
    ident.rename("product");
    REQUIRE(ident.name() == "product");

    std::ostringstream dump;
    ident.dump(dump);
    REQUIRE(dump.str() == "Identifier: name 'product'\n");
  }

  SECTION("ScopeCall") {
    // TODO: Once required features are implemented
  }

  auto dummyScope = Scope{[](auto &, const auto &) { return noneValue; }};

  SECTION("MapConstructor") {
    std::vector<ParsedMapping> mappings;
    mappings.push_back({std::make_unique<ValueExpr>(Value{7_ei}),
                        std::make_unique<ValueExpr>(Value{"Number"_es})});
    mappings.push_back({std::make_unique<ValueExpr>(Value{"key"_es}),
                        std::make_unique<ValueExpr>(Value{'v'_ec})});
    auto mapConstructor = MapConstructor{std::move(mappings)};

    REQUIRE(mapConstructor.type() == ASTNodeType::MapConstructor);

    std::ostringstream dump;
    mapConstructor.dump(dump);
    REQUIRE(dump.str() ==
            R"(MapConstructor: Mappings below
  Mapping
    ValueExpr: 7
    ValueExpr: "Number"
  Mapping
    ValueExpr: "key"
    ValueExpr: `v
)");

    auto mapValue = mapConstructor.eval(dummyScope);
    REQUIRE(mapValue.is<Map>());

    auto map = get<Map>(mapValue);
    REQUIRE(map.value.size() == 2);
    REQUIRE(map[7_ei] == Value{"Number"_es});
    REQUIRE(map["key"_es] == Value{'v'_ec});
  }

  SECTION("ListConstructor") {
    std::vector<std::unique_ptr<Expr>> elements;
    elements.push_back(std::make_unique<ValueExpr>(Value{4_ei}));
    elements.push_back(std::make_unique<ValueExpr>(Value{"element"_es}));
    elements.push_back(std::make_unique<ValueExpr>(Value{'a'_ec}));
    auto listConstructor = ListConstructor{std::move(elements)};

    REQUIRE(listConstructor.type() == ASTNodeType::ListConstructor);

    std::ostringstream dump;
    listConstructor.dump(dump);
    REQUIRE(dump.str() ==
            R"(ListConstructor: Elements below
  ValueExpr: 4
  ValueExpr: "element"
  ValueExpr: `a
)");

    auto listValue = listConstructor.eval(dummyScope);
    REQUIRE(listValue.is<List>());

    auto list = get<List>(listValue);
    REQUIRE(list.value.size() == 3);
    REQUIRE(list[0_ei] == Value{4_ei});
    REQUIRE(list[1_ei] == Value{"element"_es});
    REQUIRE(list[2_ei] == Value{'a'_ec});
  }

  SECTION("ExprList") {
    std::vector<std::unique_ptr<Expr>> expressions;
    expressions.push_back(std::make_unique<ValueExpr>(Value{4_ei}));
    expressions.push_back(std::make_unique<ValueExpr>(Value{"element"_es}));
    expressions.push_back(std::make_unique<ValueExpr>(Value{'a'_ec}));
    auto exprList = ExprList{std::move(expressions)};

    REQUIRE(exprList.type() == ASTNodeType::ExprList);

    std::ostringstream dump;
    exprList.dump(dump);
    REQUIRE(dump.str() ==
            R"(ExprList: Expressions below
  ValueExpr: 4
  ValueExpr: "element"
  ValueExpr: `a
)");

    auto result = exprList.eval(dummyScope);
    REQUIRE(result.is<Char>());

    auto c = get<Char>(result);
    REQUIRE(c.value == 'a');
  }

  SECTION("UnaryOperation") {
    auto v1 = std::make_unique<ValueExpr>(Value{7_ei});
    UnaryOperation uop{
        ASTNodeType::Custom,
        [](Scope &s, Expr &e) { return Value{-get<Int>(e.eval(s))}; },
        std::move(v1)};

    std::ostringstream dump;
    uop.dump(dump);
    REQUIRE(dump.str() ==
            R"(UnaryOperation: type 'Custom'
  ValueExpr: 7
)");

    auto result = uop.eval(dummyScope);
    REQUIRE(result.is<Int>());
    REQUIRE(get<Int>(result).value == -7);
  }

  SECTION("BinaryOperation") {
    auto v2 = std::make_unique<ValueExpr>(Value{4_ei});
    auto v3 = std::make_unique<ValueExpr>(Value{8_ei});
    BinaryOperation bop{ASTNodeType::Custom,
                        [](Scope &s, Expr &a, Expr &b) {
                          return Value{get<Int>(a.eval(s)) *
                                       get<Int>(b.eval(s))};
                        },
                        std::move(v2), std::move(v3)};
    auto result = bop.eval(dummyScope);
    REQUIRE(result.is<Int>());
    REQUIRE(get<Int>(result).value == 32);
  }
}
