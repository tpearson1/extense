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

#ifndef _LIB_EXTENSE__AST_HPP
#define _LIB_EXTENSE__AST_HPP

#include <array>
#include <memory>
#include <tuple>

#include <extense/source.hpp>
#include <extense/value.hpp>

namespace extense {
#define _LIB_EXTENSE__AST_NODE_TYPE_ENUM                                       \
  X(CustomOperator)                                                            \
  X(Semicolon)                                                                 \
  X(Colon)                                                                     \
                                                                               \
  X(Assign)                                                                    \
  X(PlusEquals)                                                                \
  X(MinusEquals)                                                               \
  X(MulEquals)                                                                 \
  X(DivEquals)                                                                 \
  X(FloorDivEquals)                                                            \
  X(PowEquals)                                                                 \
  X(ModEquals)                                                                 \
  X(BitAndEquals)                                                              \
  X(BitOrEquals)                                                               \
  X(BitXorEquals)                                                              \
  X(BitLShiftEquals)                                                           \
  X(BitRShiftEquals)                                                           \
                                                                               \
  X(BitAnd)                                                                    \
  X(BitOr)                                                                     \
  X(BitXor)                                                                    \
  X(BitLShift)                                                                 \
  X(BitRShift)                                                                 \
  X(And)                                                                       \
  X(Or)                                                                        \
  X(Equals)                                                                    \
  X(NotEquals)                                                                 \
  X(LessThan)                                                                  \
  X(LessEquals)                                                                \
  X(GreaterThan)                                                               \
  X(GreaterEquals)                                                             \
  X(Dot)                                                                       \
  X(SemicolonSemicolon)                                                        \
  X(ColonColon)                                                                \
  X(Is)                                                                        \
  X(DotDot)                                                                    \
  X(Mul)                                                                       \
  X(FloorDiv)                                                                  \
  X(Pow)                                                                       \
  X(Mod)                                                                       \
  X(Div)                                                                       \
  X(Plus)                                                                      \
  X(Minus)                                                                     \
                                                                               \
  X(IdentifierName)                                                            \
  X(UnaryPlus)                                                                 \
  X(UnaryMinus)                                                                \
  X(Not)                                                                       \
  X(Exclamation)                                                               \
  X(BitNot)                                                                    \
                                                                               \
  X(Custom)                                                                    \
  X(ValueExpr)                                                                 \
  X(LabelDeclaration)                                                          \
  X(Identifier)                                                                \
  X(ScopeCall)                                                                 \
  X(MapConstructor)                                                            \
  X(ListConstructor)                                                           \
  X(ExprList)

enum class ASTNodeType {
#define X(a) a,
  _LIB_EXTENSE__AST_NODE_TYPE_ENUM
#undef X
};

std::ostream &operator<<(std::ostream &os, ASTNodeType type);

constexpr bool isUnaryOperator(ASTNodeType t) {
  auto typeOrd = static_cast<int>(t);
  return typeOrd >= static_cast<int>(ASTNodeType::IdentifierName) &&
         typeOrd <= static_cast<int>(ASTNodeType::BitNot);
}

constexpr bool isBinaryOperator(ASTNodeType t) {
  auto typeOrd = static_cast<int>(t);
  return typeOrd >= static_cast<int>(ASTNodeType::Assign) &&
         typeOrd <= static_cast<int>(ASTNodeType::Minus);
}

inline void makeIndent(std::ostream &os, int amount) {
  for (int i = 0; i < amount; i++) os << ' ';
}

inline constexpr int indentAmount = 2;

class ExceptionWrapper {
  Exception data_;
  Source::Location location_;

public:
  ExceptionWrapper(Exception e, Source::Location loc)
      : data_(std::move(e)), location_(std::move(loc)) {}

  const Exception &data() const { return data_; }
  const Source::Location &location() const { return location_; }
  const std::string &type() const { return data_.type(); }
};

class Expr {
private:
  ASTNodeType type_;
  Source::Location location_;

protected:
  void setType(ASTNodeType type) { type_ = type; }

  void displayHeaderWithIndent(std::ostream &os, int indent) const;

public:
  Expr(Source::Location location, ASTNodeType type)
      : type_(type), location_(std::move(location)) {}

  ASTNodeType type() const { return type_; }
  Source::Location location() const { return location_; }

  void displayHeader(std::ostream &os) const { displayHeaderWithIndent(os, 0); }

  void dump(std::ostream &os) { dumpWithIndent(os, 0); }
  virtual void dumpWithIndent(std::ostream &os, int indent) const = 0;

protected:
  virtual Proxy evalImpl(Scope &scope) = 0;

public:
  Proxy eval(Scope &scope);

  virtual ~Expr() {}
};

Value constEval(Scope &scope, Expr &e);

class ValueExpr : public Expr {
  Value value_;

protected:
  Proxy evalImpl(Scope &) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  ValueExpr(Source::Location location, Value v)
      : Expr(std::move(location), ASTNodeType::ValueExpr),
        value_(std::move(v)) {}

  const Value &value() const { return value_; }
  Value &value() { return value_; }
};

class LabelDeclaration : public Expr {
  std::string name_;

protected:
  Proxy evalImpl(Scope &) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  LabelDeclaration(Source::Location location, std::string name)
      : Expr(std::move(location), ASTNodeType::LabelDeclaration),
        name_(std::move(name)) {}

  const std::string &name() const { return name_; }
  void rename(std::string name) { name_ = std::move(name); }
};

/*
 * Exception thrown when attempting to access an undefined identifier.
 */
class IdentifierError : public Exception {
  std::string identifier_;

public:
  explicit IdentifierError(std::string ident)
      : Exception("Undefined identifier"), identifier_(std::move(ident)) {
    setType("IdentifierError");
  }

  const std::string &identifier() const { return identifier_; }
};

class Identifier : public Expr {
  std::string name_;

protected:
  Proxy evalImpl(Scope &s) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  Identifier(Source::Location location, std::string name)
      : Expr(std::move(location), ASTNodeType::Identifier),
        name_(std::move(name)) {}

  const std::string &name() const { return name_; }
  void rename(std::string name) { name_ = std::move(name); }
};

class ScopeCall : public Expr {
  std::unique_ptr<Expr> scope_;
  std::unique_ptr<Expr> argument_;

protected:
  Proxy evalImpl(Scope &) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  ScopeCall(Source::Location location, std::unique_ptr<Expr> scope,
            std::unique_ptr<Expr> argument)
      : Expr(std::move(location), ASTNodeType::ScopeCall),
        scope_(std::move(scope)), argument_(std::move(argument)) {}

  const Expr &scope() const { return *scope_; }
  Expr &scope() { return *scope_; }

  const Expr &argument() const { return *argument_; }
  Expr &argument() { return *argument_; }
};

struct ParsedMapping {
  std::unique_ptr<Expr> key, value;
};

class MapConstructor : public Expr {
  std::vector<ParsedMapping> mappings_;

protected:
  // Returns a Map created from the evaluated mappings
  Proxy evalImpl(Scope &) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  MapConstructor(Source::Location location, std::vector<ParsedMapping> mappings)
      : Expr(std::move(location), ASTNodeType::MapConstructor),
        mappings_(std::move(mappings)) {}
};

class ListConstructor : public Expr {
  std::vector<std::unique_ptr<Expr>> elements_;

protected:
  // Returns a List constructed from the evaluated elements
  Proxy evalImpl(Scope &) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  ListConstructor(Source::Location location,
                  std::vector<std::unique_ptr<Expr>> elements)
      : Expr(std::move(location), ASTNodeType::ListConstructor),
        elements_(std::move(elements)) {}
};

class ExprList : public Expr {
  using ExprContainer = std::vector<std::unique_ptr<Expr>>;
  ExprContainer exprs_;
  std::vector<Label> labels;

protected:
  Proxy evalImpl(Scope &outer) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  ExprList(Source::Location location, std::vector<std::unique_ptr<Expr>> exprs);

  Scope toScope(Scope &outer);

  using const_iterator = ExprContainer::const_iterator;

  const_iterator begin() const { return exprs_.begin(); }
  const_iterator end() const { return exprs_.end(); }
};

class UnaryOperation : public Expr {
protected:
  Proxy evalImpl(Scope &s) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  using Function = Value(Scope &, Expr &);

  UnaryOperation(Source::Location location, ASTNodeType opType,
                 Function *operation, std::unique_ptr<Expr> operand)
      : Expr(std::move(location), opType), operation_(operation),
        operand_(std::move(operand)) {}

  const Expr &operand() const { return *operand_; }
  Expr &operand() { return *operand_; }

private:
  Function *operation_;
  std::unique_ptr<Expr> operand_;
};

class BinaryOperation : public Expr {
protected:
  Proxy evalImpl(Scope &s) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  using Function = Value(Scope &, Expr &, Expr &);

  BinaryOperation(Source::Location location, ASTNodeType opType,
                  Function *operation, std::unique_ptr<Expr> operand1,
                  std::unique_ptr<Expr> operand2)
      : Expr(std::move(location), opType), operation_(operation),
        operand1_(std::move(operand1)), operand2_(std::move(operand2)) {}

  const Expr &leftOperand() const { return *operand1_; }
  Expr &leftOperand() { return *operand1_; }

  const Expr &rightOperand() const { return *operand2_; }
  Expr &rightOperand() { return *operand2_; }

private:
  Function *operation_;
  std::unique_ptr<Expr> operand1_, operand2_;
};

class CustomOperation : public Expr {
  std::string op_;

protected:
  Proxy evalImpl(Scope &s) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  CustomOperation(Source::Location location, std::string op,
                  std::unique_ptr<Expr> operand1,
                  std::unique_ptr<Expr> operand2)
      : Expr(std::move(location), ASTNodeType::CustomOperator),
        op_(std::move(op)), operand1_(std::move(operand1)),
        operand2_(std::move(operand2)) {}

  const std::string &operatorString() const { return op_; }

  const Expr &leftOperand() const { return *operand1_; }
  Expr &leftOperand() { return *operand1_; }

  const Expr &rightOperand() const { return *operand2_; }
  Expr &rightOperand() { return *operand2_; }

private:
  std::unique_ptr<Expr> operand1_, operand2_;
};

class IndexOperation : public Expr {
  std::unique_ptr<Expr> operand1_, operand2_;

protected:
  Proxy evalImpl(Scope &) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  IndexOperation(Source::Location location, ASTNodeType opType,
                 std::unique_ptr<Expr> operand1, std::unique_ptr<Expr> operand2)
      : Expr(std::move(location), opType), operand1_(std::move(operand1)),
        operand2_(std::move(operand2)) {}

  const Expr &leftOperand() const { return *operand1_; }
  Expr &leftOperand() { return *operand1_; }

  const Expr &rightOperand() const { return *operand2_; }
  Expr &rightOperand() { return *operand2_; }
};

namespace detail {
Value buildArgumentsForScopeCall(const Value &lhs, const Value &rhs);
}
} // namespace extense

#endif // _LIB_EXTENSE__AST_HPP
