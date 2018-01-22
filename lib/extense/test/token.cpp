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

#include <extense/token.hpp>

#include <catch.hpp>

using namespace extense;

TEST_CASE("tryMatch works", "[detail::tryMatch]") {
  Source s{"Find the pattern"};

  bool res = detail::tryMatch(s, "att");
  REQUIRE(!res);
  REQUIRE(s.index() == 0);

  res = detail::tryMatch(s, "Fy");
  REQUIRE(!res);
  REQUIRE(s.index() == 0);

  res = detail::tryMatch(s, "Fi");
  REQUIRE(res);
  REQUIRE(s.index() == 2);
}

TEST_CASE("skipWhitespace works", "[detail::skipWhitespace]") {
  Source s{"\t\t    \t  1        \n   k"};

  detail::skipWhitespace(s);
  REQUIRE(s.currentChar() == '1');

  detail::skipWhitespace(s);
  REQUIRE(s.currentChar() == '1');

  s.nextChar();

  detail::skipWhitespace(s);
  REQUIRE(s.currentChar() == '\n');

  s.nextChar();

  detail::skipWhitespace(s);
  REQUIRE(s.currentChar() == 'k');

  s.nextChar();

  detail::skipWhitespace(s);
  REQUIRE(s.currentChar().isAfterSource());
}

TEST_CASE("Lexing detail functions",
          "[detail::lexCharacter, detail::lexString, detail::lexLabel, "
          "detail::lexUnsigned, detail::lexInteger, detail::lexNumber, "
          "detail::lexIdentifier, detail::lexOperator]") {
  Token t{Source::Location{}};

  SECTION("lexCharacter") {
    Source s{"`a"};
    REQUIRE(detail::lexCharacter(s, t));
    REQUIRE(t.type() == Token::Type::Character);
    REQUIRE(s.currentChar().isAfterSource());
    t.setType(Token::Type::Plus);

    Source s2{"`\\nk"};
    REQUIRE(detail::lexCharacter(s2, t));
    REQUIRE(t.type() == Token::Type::Character);
    REQUIRE(s2.currentChar() == 'k');
    t.setType(Token::Type::Plus);

    Source s3{"Word"};
    REQUIRE(!detail::lexCharacter(s3, t));
    REQUIRE(t.type() == Token::Type::Plus);
    REQUIRE(s3.index() == 0);
  }

  SECTION("lexString") {
    Source s{"\"Hello, World!\"_"};
    REQUIRE(detail::lexString(s, t));
    REQUIRE(t.type() == Token::Type::String);
    REQUIRE(s.currentChar() == '_');
    t.setType(Token::Type::Plus);

    Source s2{"'Hello, World!'_"};
    REQUIRE(detail::lexString(s2, t));
    REQUIRE(t.type() == Token::Type::String);
    REQUIRE(s2.currentChar() == '_');
    t.setType(Token::Type::Plus);

    Source s3{R"("   \""_)"};
    REQUIRE(detail::lexString(s3, t));
    REQUIRE(t.type() == Token::Type::String);
    REQUIRE(s3.currentChar() == '_');
    t.setType(Token::Type::Plus);

    Source s4{R"('   \''_)"};
    REQUIRE(detail::lexString(s4, t));
    REQUIRE(t.type() == Token::Type::String);
    REQUIRE(s4.currentChar() == '_');
    t.setType(Token::Type::Plus);

    Source s5{"Certainly not a string"};
    REQUIRE(!detail::lexString(s5, t));
    REQUIRE(t.type() == Token::Type::Plus);
    REQUIRE(s5.currentChar() == 'C');

    Source s6{"\""};
    bool correct = false;
    try {
      detail::lexString(s6, t);
    } catch (const LexingError &) { correct = true; }
    REQUIRE(correct);
    correct = false;

    Source s7{"'"};
    try {
      detail::lexString(s7, t);
    } catch (const LexingError &) { correct = true; }
    REQUIRE(correct);
  }

  SECTION("lexLabel") {
    Source s{"@_label "};
    REQUIRE(detail::lexLabel(s, t));
    REQUIRE(t.type() == Token::Type::Label);
    REQUIRE(s.currentChar() == ' ');
    t.setType(Token::Type::Plus);

    Source s2{"@lab3l "};
    REQUIRE(detail::lexLabel(s2, t));
    REQUIRE(t.type() == Token::Type::Label);
    REQUIRE(s2.currentChar() == ' ');
    t.setType(Token::Type::Plus);

    Source s3{"@1"};
    bool correct = false;
    try {
      detail::lexLabel(s3, t);
    } catch (const LexingError &) { correct = true; }
    REQUIRE(correct);

    Source s4{"Not a label"};
    REQUIRE(!detail::lexLabel(s4, t));
    REQUIRE(t.type() == Token::Type::Plus);
    REQUIRE(s4.currentChar() == 'N');
  }

  SECTION("lexUnsigned") {
    Source s{"1234_"};
    REQUIRE(detail::lexUnsigned(s));
    REQUIRE(s.currentChar() == '_');

    Source s2{"-1234"};
    REQUIRE(!detail::lexUnsigned(s2));
    REQUIRE(s2.currentChar() == '-');

    Source s3{"seventy"};
    REQUIRE(!detail::lexUnsigned(s3));
    REQUIRE(s3.currentChar() == 's');
  }

  SECTION("lexInteger") {
    Source s{"1234_"};
    REQUIRE(detail::lexInteger(s, t));
    REQUIRE(s.currentChar() == '_');
    REQUIRE(t.type() == Token::Type::Integer);
    t.setType(Token::Type::Plus);

    Source s2{"-1234."};
    REQUIRE(detail::lexInteger(s2, t));
    REQUIRE(s2.currentChar() == '.');
    REQUIRE(t.type() == Token::Type::Integer);
    t.setType(Token::Type::Plus);

    Source s3{"seventy"};
    REQUIRE(!detail::lexInteger(s3, t));
    REQUIRE(s3.currentChar() == 's');
    REQUIRE(t.type() == Token::Type::Plus);

    Source s4{"+1."};
    REQUIRE(detail::lexInteger(s4, t));
    REQUIRE(s4.currentChar() == '.');
    REQUIRE(t.type() == Token::Type::Integer);
    t.setType(Token::Type::Plus);

    Source s5{"+|"};
    REQUIRE(!detail::lexInteger(s5, t));
    REQUIRE(s5.currentChar() == '+');
    REQUIRE(t.type() == Token::Type::Plus);
  }

  SECTION("lexNumber") {
    SECTION("Int") {
      Source s{"1234_"};
      REQUIRE(detail::lexNumber(s, t));
      REQUIRE(s.currentChar() == '_');
      REQUIRE(t.type() == Token::Type::Integer);
      t.setType(Token::Type::Plus);

      Source s2{"-1234."};
      REQUIRE(detail::lexNumber(s2, t));
      REQUIRE(s2.currentChar() == '.');
      REQUIRE(t.type() == Token::Type::Integer);
      t.setType(Token::Type::Plus);

      Source s3{"seventy"};
      REQUIRE(!detail::lexNumber(s3, t));
      REQUIRE(s3.currentChar() == 's');
      REQUIRE(t.type() == Token::Type::Plus);

      Source s4{"-3.-26e2|"};
      REQUIRE(detail::lexNumber(s4, t));
      REQUIRE(s4.currentChar() == '.');
      REQUIRE(t.type() == Token::Type::Integer);
      t.setType(Token::Type::Plus);
    }

    SECTION("Float") {
      Source s{"-3.26|"};
      REQUIRE(detail::lexNumber(s, t));
      REQUIRE(s.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::Float);
      t.setType(Token::Type::Plus);

      Source s2{"-3.26e2|"};
      REQUIRE(detail::lexNumber(s2, t));
      REQUIRE(s2.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::Float);
      t.setType(Token::Type::Plus);

      Source s3{"-3.26e-2|"};
      REQUIRE(detail::lexNumber(s3, t));
      REQUIRE(s3.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::Float);
      t.setType(Token::Type::Plus);

      Source s4{"-3e4|"};
      REQUIRE(detail::lexNumber(s4, t));
      REQUIRE(s4.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::Float);
      t.setType(Token::Type::Plus);

      Source s5{"7e-4|"};
      REQUIRE(detail::lexNumber(s5, t));
      REQUIRE(s5.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::Float);

      Source s6{"-3.26eHi|"};
      bool correct = false;
      try {
        detail::lexNumber(s6, t);
      } catch (const LexingError &) { correct = true; }
      REQUIRE(correct);
    }
  }

  SECTION("lexIdentifier") {
    Source s{"_ident "};
    REQUIRE(detail::lexIdentifier(s, t));
    REQUIRE(t.type() == Token::Type::Identifier);
    REQUIRE(s.currentChar() == ' ');
    t.setType(Token::Type::Plus);

    Source s2{"id3nt "};
    REQUIRE(detail::lexIdentifier(s2, t));
    REQUIRE(t.type() == Token::Type::Identifier);
    REQUIRE(s2.currentChar() == ' ');
    t.setType(Token::Type::Plus);

    Source s3{"1"};
    REQUIRE(!detail::lexIdentifier(s3, t));
    REQUIRE(t.type() == Token::Type::Plus);
    REQUIRE(s3.currentChar() == '1');

    Source s4{"Not an identifier"}; // 'Not' actually could be an identifier
    REQUIRE(detail::lexIdentifier(s4, t));
    REQUIRE(t.type() == Token::Type::Identifier);
    REQUIRE(s4.currentChar() == ' ');
    t.setType(Token::Type::Plus);

    SECTION("boolean values") {
      Source s{"true|"};
      REQUIRE(detail::lexIdentifier(s, t));
      REQUIRE(s.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::Bool);
      t.setType(Token::Type::Plus);

      Source s2{"false "};
      REQUIRE(detail::lexIdentifier(s2, t));
      REQUIRE(s2.currentChar() == ' ');
      REQUIRE(t.type() == Token::Type::Bool);
      t.setType(Token::Type::Plus);

      Source s3{"falseeslaf"};
      REQUIRE(detail::lexIdentifier(s3, t));
      REQUIRE(s3.currentChar().isAfterSource());
      REQUIRE(t.type() == Token::Type::Identifier);
      t.setType(Token::Type::Plus);
    }

    SECTION("logical operators") {
      Source s{"and|"};
      REQUIRE(detail::lexIdentifier(s, t));
      REQUIRE(s.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::And);
      t.setType(Token::Type::Plus);

      Source s2{"andy^"};
      REQUIRE(detail::lexIdentifier(s2, t));
      REQUIRE(s2.currentChar() == '^');
      REQUIRE(t.type() == Token::Type::Identifier);
      t.setType(Token::Type::Plus);

      Source s3{"or"};
      REQUIRE(detail::lexIdentifier(s3, t));
      REQUIRE(s3.currentChar().isAfterSource());
      REQUIRE(t.type() == Token::Type::Or);
      t.setType(Token::Type::Plus);

      Source s4{"not&"};
      REQUIRE(detail::lexIdentifier(s4, t));
      REQUIRE(s4.currentChar() == '&');
      REQUIRE(t.type() == Token::Type::Not);
      t.setType(Token::Type::Plus);

      Source s5{"no"};
      REQUIRE(detail::lexIdentifier(s5, t));
      REQUIRE(s5.currentChar().isAfterSource());
      REQUIRE(t.type() == Token::Type::Identifier);
      t.setType(Token::Type::Plus);
    }

    SECTION("None") {
      Source s{"Nones|"};
      REQUIRE(detail::lexIdentifier(s, t));
      REQUIRE(s.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::Identifier);
      t.setType(Token::Type::Plus);

      Source s2{"None|"};
      REQUIRE(detail::lexIdentifier(s2, t));
      REQUIRE(s2.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::None);
    }
  }

  SECTION("lexOperator") {
    Source s{"= "};
    REQUIRE(detail::lexOperator(s, t));
    REQUIRE(t.type() == Token::Type::Assign);
    REQUIRE(s.currentChar() == ' ');
    t.setType(Token::Type::Identifier);

    Source s2{"+ "};
    REQUIRE(detail::lexOperator(s2, t));
    REQUIRE(t.type() == Token::Type::Plus);
    REQUIRE(s2.currentChar() == ' ');
    t.setType(Token::Type::Identifier);

    Source s3{"-> "};
    REQUIRE(detail::lexOperator(s3, t));
    REQUIRE(t.type() == Token::Type::MapsTo);
    REQUIRE(s3.currentChar() == ' ');
    t.setType(Token::Type::Identifier);

    Source s4{"operator"};
    REQUIRE(!detail::lexOperator(s4, t));
    REQUIRE(t.type() == Token::Type::Identifier);
    REQUIRE(s4.currentChar() == 'o');
  }
}

TEST_CASE("Skipping past characters",
          "[detail::skipPastPermitEOS, detail::skipPast]") {
  Source s{"Dummy text"};
  detail::skipPast(s, [](auto c) { return c == 't'; });
  REQUIRE(s.currentChar() == 'e');

  Source s2{"More dummy text"};
  detail::skipPastPermitEOS(s2, [](auto c) { return c == 'z'; });
  REQUIRE(s2.currentChar().isAfterSource());

  SECTION("Testing throwing on failure") {
    Source s3{"abcdefg"};

    bool correct = false;
    try {
      detail::skipPast(s3, [](auto c) { return c == 'h'; });
    } catch (const LexingError &error) { correct = true; }

    REQUIRE(correct);
  }
}

TEST_CASE("Token::Type correct ostream output", "[Token::Type]") {
  std::ostringstream out;
  out << Token::Type::ModEquals;
  REQUIRE(out.str() == "ModEquals");
}
