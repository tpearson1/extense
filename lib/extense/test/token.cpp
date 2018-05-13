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

#include <cmath>
#include <ostream>

#include "common.hpp"
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

TEST_CASE(
    "Lexing detail functions",
    "[detail::lexEscapeSequence, detail::lexCharacter, detail::lexString, "
    "detail::lexLabel, detail::lexInteger, detail::lexNumber, "
    "detail::lexIdentifier, detail::lexOperator]") {
  SECTION("lexEscapeSequence") {
    char result = '\0';
    Source s{R"(\' )"};
    detail::lexEscapeSequence(s, result);
    REQUIRE(result == '\'');
    REQUIRE(s.currentChar() == ' ');

    result = '\0';
    Source s2{R"(\" )"};
    detail::lexEscapeSequence(s2, result);
    REQUIRE(result == '"');
    REQUIRE(s2.currentChar() == ' ');

    result = '\0';
    Source s3{R"(\\ )"};
    detail::lexEscapeSequence(s3, result);
    REQUIRE(result == '\\');
    REQUIRE(s3.currentChar() == ' ');

    result = '\0';
    Source s4{R"(\a )"};
    detail::lexEscapeSequence(s4, result);
    REQUIRE(result == '\a');
    REQUIRE(s4.currentChar() == ' ');

    result = '\0';
    Source s5{R"(\b )"};
    detail::lexEscapeSequence(s5, result);
    REQUIRE(result == '\b');
    REQUIRE(s5.currentChar() == ' ');

    result = '\0';
    Source s6{R"(\f )"};
    detail::lexEscapeSequence(s6, result);
    REQUIRE(result == '\f');
    REQUIRE(s6.currentChar() == ' ');

    result = '\0';
    Source s7{R"(\n )"};
    detail::lexEscapeSequence(s7, result);
    REQUIRE(result == '\n');
    REQUIRE(s7.currentChar() == ' ');

    result = '\0';
    Source s8{R"(\r )"};
    detail::lexEscapeSequence(s8, result);
    REQUIRE(result == '\r');
    REQUIRE(s8.currentChar() == ' ');

    result = '\0';
    Source s9{R"(\t )"};
    detail::lexEscapeSequence(s9, result);
    REQUIRE(result == '\t');
    REQUIRE(s9.currentChar() == ' ');

    result = '\0';
    Source s10{R"(\v )"};
    detail::lexEscapeSequence(s10, result);
    REQUIRE(result == '\v');
    REQUIRE(s10.currentChar() == ' ');

    // ASCII characters with octal values
    result = '\0';
    Source s11{R"(\036 )"};
    detail::lexEscapeSequence(s11, result);
    REQUIRE(result == '\36');
    REQUIRE(s11.currentChar() == ' ');

    result = '\0';
    Source s12{R"(\36 )"};
    detail::lexEscapeSequence(s12, result);
    REQUIRE(result == '\36');
    REQUIRE(s12.currentChar() == ' ');

    result = '\0';
    Source s13{R"(\142 )"};
    detail::lexEscapeSequence(s13, result);
    REQUIRE(result == '\142');
    REQUIRE(s13.currentChar() == ' ');

    // ASCII characters with hexadecimal values
    result = '\0';
    Source s14{R"(\xFA )"};
    detail::lexEscapeSequence(s14, result);
    REQUIRE(result == '\xFA');
    REQUIRE(s14.currentChar() == ' ');

    result = '\0';
    Source s15{R"(\xB )"};
    detail::lexEscapeSequence(s15, result);
    REQUIRE(result == '\xB');
    REQUIRE(s15.currentChar() == ' ');

    result = '\0';
    Source s16{R"(\x3E )"};
    detail::lexEscapeSequence(s16, result);
    REQUIRE(result == '\x3E');
    REQUIRE(s16.currentChar() == ' ');

    // ASCII characters with decimal values
    result = '\0';
    Source s17{R"(\d22 )"};
    detail::lexEscapeSequence(s17, result);
    REQUIRE(result == '\x16');
    REQUIRE(s16.currentChar() == ' ');

    result = '\0';
    Source s18{R"(\d57 )"};
    detail::lexEscapeSequence(s18, result);
    REQUIRE(result == '\x39');
    REQUIRE(s18.currentChar() == ' ');

    result = '\0';
    Source s19{R"(\d255 )"};
    detail::lexEscapeSequence(s19, result);
    REQUIRE(result == '\xFF');
    REQUIRE(s19.currentChar() == ' ');

    // Unexpected end of source
    Source s20{R"(\)"};
    bool correct = false;
    try {
      detail::lexEscapeSequence(s20, result);
    } catch (const LexingError &) { correct = true; }
    REQUIRE(correct);

    // Unrecognized escape sequence
    Source s21{R"(\p)"};
    correct = false;
    try {
      detail::lexEscapeSequence(s21, result);
    } catch (const LexingError &) { correct = true; }
    REQUIRE(correct);
  }

  Token t{Source::Location{}};
  t.setType(Token::Type::EndSource);

  SECTION("lexCharacter") {
    Source s{"`a"};
    REQUIRE(detail::lexCharacter(s, t));
    REQUIRE(t.type() == Token::Type::Character);
    REQUIRE(s.currentChar().isAfterSource());
    REQUIRE(variantEquals(t.data(), 'a'));
    t.setType(Token::Type::Plus);

    Source s2{"`\\nk"};
    REQUIRE(detail::lexCharacter(s2, t));
    REQUIRE(t.type() == Token::Type::Character);
    REQUIRE(s2.currentChar() == 'k');
    REQUIRE(variantEquals(t.data(), '\n'));
    t.setType(Token::Type::Plus);

    Source s3{"`\\x52l"};
    REQUIRE(detail::lexCharacter(s3, t));
    REQUIRE(t.type() == Token::Type::Character);
    REQUIRE(s3.currentChar() == 'l');
    REQUIRE(variantEquals(t.data(), '\x52'));
    t.setType(Token::Type::Plus);

    Source s4{"Word"};
    REQUIRE(!detail::lexCharacter(s4, t));
    REQUIRE(t.type() == Token::Type::Plus);
    REQUIRE(s4.index() == 0);
  }

  SECTION("lexString") {
    Source s{"\"Hello, World!\"_"};
    REQUIRE(detail::lexString(s, t));
    REQUIRE(t.type() == Token::Type::String);
    REQUIRE(s.currentChar() == '_');
    REQUIRE(variantEquals(t.data(), std::string("Hello, World!")));
    t.setType(Token::Type::Plus);

    Source s2{"'Hello, World!'_"};
    REQUIRE(detail::lexString(s2, t));
    REQUIRE(t.type() == Token::Type::String);
    REQUIRE(s2.currentChar() == '_');
    REQUIRE(variantEquals(t.data(), std::string("Hello, World!")));
    t.setType(Token::Type::Plus);

    Source s3{R"("   \""_)"};
    REQUIRE(detail::lexString(s3, t));
    REQUIRE(t.type() == Token::Type::String);
    REQUIRE(s3.currentChar() == '_');
    REQUIRE(variantEquals(t.data(), std::string("   \"")));
    t.setType(Token::Type::Plus);

    Source s4{R"('   \''_)"};
    REQUIRE(detail::lexString(s4, t));
    REQUIRE(t.type() == Token::Type::String);
    REQUIRE(s4.currentChar() == '_');
    REQUIRE(variantEquals(t.data(), std::string("   '")));
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

    t.setType(Token::Type::Plus);
    Source s8{"'This is a tab: \t' "};
    REQUIRE(detail::lexString(s8, t));
    REQUIRE(t.type() == Token::Type::String);
    REQUIRE(variantEquals(t.data(), std::string("This is a tab: \t")));
    REQUIRE(s8.currentChar() == ' ');
  }

  SECTION("lexLabel") {
    Source s{"@_label "};
    REQUIRE(detail::lexLabel(s, t));
    REQUIRE(t.type() == Token::Type::LabelDeclaration);
    REQUIRE(s.currentChar() == ' ');
    t.setType(Token::Type::Plus);

    Source s2{"@lab3l "};
    REQUIRE(detail::lexLabel(s2, t));
    REQUIRE(t.type() == Token::Type::LabelDeclaration);
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

  SECTION("lexNumber") {
    SECTION("Int") {
      Source s{"1234_"};
      REQUIRE(detail::lexNumber(s, t));
      REQUIRE(s.currentChar() == '_');
      REQUIRE(t.type() == Token::Type::Integer);
      REQUIRE(variantEquals<std::int64_t>(t.data(), 1234));
      t.setType(Token::Type::Plus);

      Source s2{"1234."};
      REQUIRE(detail::lexNumber(s2, t));
      REQUIRE(s2.currentChar() == '.');
      REQUIRE(t.type() == Token::Type::Integer);
      REQUIRE(variantEquals<std::int64_t>(t.data(), 1234));
      t.setType(Token::Type::Plus);

      Source s3{"seventy"};
      REQUIRE(!detail::lexNumber(s3, t));
      REQUIRE(s3.currentChar() == 's');
      REQUIRE(t.type() == Token::Type::Plus);

      Source s4{"3.-26e2|"};
      REQUIRE(detail::lexNumber(s4, t));
      REQUIRE(s4.currentChar() == '.');
      REQUIRE(t.type() == Token::Type::Integer);
      REQUIRE(variantEquals<std::int64_t>(t.data(), 3));
      t.setType(Token::Type::Plus);

      Source s5{"034!"};
      REQUIRE(detail::lexNumber(s5, t));
      REQUIRE(s5.currentChar() == '!');
      REQUIRE(t.type() == Token::Type::Integer);
      REQUIRE(variantEquals<std::int64_t>(t.data(), 3 * 8 + 4));
      t.setType(Token::Type::Plus);

      Source s6{"0xA4!"};
      REQUIRE(detail::lexNumber(s6, t));
      REQUIRE(s6.currentChar() == '!');
      REQUIRE(t.type() == Token::Type::Integer);
      REQUIRE(variantEquals<std::int64_t>(t.data(), 10 * 16 + 4));
      t.setType(Token::Type::Plus);

      Source s7{"0b11034!"};
      REQUIRE(detail::lexNumber(s7, t));
      REQUIRE(s7.currentChar() == '3');
      REQUIRE(t.type() == Token::Type::Integer);
      REQUIRE(variantEquals<std::int64_t>(t.data(), 4 + 2));
      t.setType(Token::Type::Plus);

      Source s8{"0x7.4|"};
      REQUIRE(detail::lexNumber(s8, t));
      REQUIRE(s8.currentChar() == '.');
      REQUIRE(t.type() == Token::Type::Integer);
      REQUIRE(variantEquals<std::int64_t>(t.data(), 7));

      Source s9{"0x"};
      bool correct = false;
      try {
        detail::lexNumber(s9, t);
      } catch (const LexingError &) { correct = true; }
      REQUIRE(correct);

      Source s10{"0b"};
      correct = false;
      try {
        detail::lexNumber(s10, t);
      } catch (const LexingError &) { correct = true; }
      REQUIRE(correct);

      t.setType(Token::Type::Plus);
    }

    SECTION("Float") {
      auto nearlyEquals = [](auto a, auto b) {
        constexpr const auto floatTolerance = 0.00000001;
        return std::abs(a - b) < floatTolerance;
      };

      Source s{"3.26|"};
      REQUIRE(detail::lexNumber(s, t));
      REQUIRE(s.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::Float);
      INFO(std::get<double>(t.data()));
      REQUIRE(variantEquals<double>(t.data(), 3.26, nearlyEquals));
      t.setType(Token::Type::Plus);

      Source s2{"3.26e2|"};
      REQUIRE(detail::lexNumber(s2, t));
      REQUIRE(s2.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::Float);
      REQUIRE(variantEquals<double>(t.data(), 326.0, nearlyEquals));
      t.setType(Token::Type::Plus);

      Source s3{"3.26e-2|"};
      REQUIRE(detail::lexNumber(s3, t));
      REQUIRE(s3.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::Float);
      REQUIRE(variantEquals<double>(t.data(), 0.0326, nearlyEquals));
      t.setType(Token::Type::Plus);

      Source s4{"3e4|"};
      REQUIRE(detail::lexNumber(s4, t));
      REQUIRE(s4.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::Float);
      REQUIRE(variantEquals<double>(t.data(), 30000.0, nearlyEquals));
      t.setType(Token::Type::Plus);

      Source s5{"7e-4|"};
      REQUIRE(detail::lexNumber(s5, t));
      REQUIRE(s5.currentChar() == '|');
      REQUIRE(t.type() == Token::Type::Float);
      REQUIRE(variantEquals<double>(t.data(), 0.0007, nearlyEquals));

      Source s6{"3.26eHi|"};
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
      REQUIRE(variantEquals(t.data(), true));
      t.setType(Token::Type::Plus);

      Source s2{"false "};
      REQUIRE(detail::lexIdentifier(s2, t));
      REQUIRE(s2.currentChar() == ' ');
      REQUIRE(t.type() == Token::Type::Bool);
      REQUIRE(variantEquals(t.data(), false));
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
