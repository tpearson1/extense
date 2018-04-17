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
  X(Assign)                                                                    \
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
  X(Semicolon)                                                                 \
  X(Colon)                                                                     \
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
  explicit Expr(Source::Location location, ASTNodeType type)
      : type_(type), location_(std::move(location)) {}

  ASTNodeType type() const { return type_; }
  Source::Location location() const { return location_; }

  void displayHeader(std::ostream &os) const { displayHeaderWithIndent(os, 0); }

  void dump(std::ostream &os) { dumpWithIndent(os, 0); }
  virtual void dumpWithIndent(std::ostream &os, int indent) const = 0;

  struct EvalResult {
    bool isMutable = false;
    Value value;
  };

protected:
  virtual EvalResult evalImpl(Scope &scope) = 0;
  virtual Value *tryMutableEvalImpl(Scope &) { return nullptr; }

  // For mutating elements in a string
  virtual Char *tryMutableCharEvalImpl(Scope &) { return nullptr; }

public:
  EvalResult eval(Scope &scope);

  Value *tryMutableEval(Scope &scope);

  // For mutating elements in a string
  Char *tryMutableCharEval(Scope &scope);

  virtual ~Expr() {}
};

inline Value constEval(Scope &scope, Expr &e) { return e.eval(scope).value; }

template <typename OpFunc>
auto mutableEval(Scope &scope, Expr &a, OpFunc f) {
  auto *lhs = a.tryMutableEval(scope);
  if (lhs) return f(*lhs);
  auto *charLhs = a.tryMutableCharEval(scope);
  if (charLhs) {
    auto toAssign = Value{*charLhs};
    auto ret = f(toAssign);
    if (!toAssign.is<Char>()) {
      throw InvalidBinaryOperation{"Mutable Char", toAssign.typeAsString(),
                                   "Expected Char in assignment"};
    }
    *charLhs = get<Char>(toAssign);
    return ret;
  }

  auto [isMutable, value] = a.eval(scope);
  if (!isMutable || !value.is<Reference>())
    throw InvalidBinaryOperation{"Constant expression", "<Unknown>",
                                 "Expected mutable expression"};
  return f(value);
}

class ValueExpr : public Expr {
  Value value_;

protected:
  EvalResult evalImpl(Scope &) override { return {false, value_}; }

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  explicit ValueExpr(Source::Location location, Value v)
      : Expr(std::move(location), ASTNodeType::ValueExpr),
        value_(std::move(v)) {}

  const Value &value() const { return value_; }
  Value &value() { return value_; }
};

class LabelDeclaration : public Expr {
  std::string name_;

protected:
  EvalResult evalImpl(Scope &) override { return {false, noneValue}; }

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  explicit LabelDeclaration(Source::Location location, std::string name)
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
  EvalResult evalImpl(Scope &s) override;
  Value *tryMutableEvalImpl(Scope &s) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  explicit Identifier(Source::Location location, std::string name)
      : Expr(std::move(location), ASTNodeType::Identifier),
        name_(std::move(name)) {}

  const std::string &name() const { return name_; }
  void rename(std::string name) { name_ = std::move(name); }
};

class ScopeCall : public Expr {
  std::unique_ptr<Expr> scope_;
  std::unique_ptr<Expr> argument_;

protected:
  EvalResult evalImpl(Scope &) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  explicit ScopeCall(Source::Location location, std::unique_ptr<Expr> scope,
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
  EvalResult evalImpl(Scope &) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  explicit MapConstructor(Source::Location location,
                          std::vector<ParsedMapping> mappings)
      : Expr(std::move(location), ASTNodeType::MapConstructor),
        mappings_(std::move(mappings)) {}
};

class ListConstructor : public Expr {
  std::vector<std::unique_ptr<Expr>> elements_;

protected:
  // Returns a List constructed from the evaluated elements
  EvalResult evalImpl(Scope &) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  explicit ListConstructor(Source::Location location,
                           std::vector<std::unique_ptr<Expr>> elements)
      : Expr(std::move(location), ASTNodeType::ListConstructor),
        elements_(std::move(elements)) {}
};

class ExprList : public Expr {
  using ExprContainer = std::vector<std::unique_ptr<Expr>>;
  ExprContainer exprs_;
  std::vector<Label> labels;

protected:
  EvalResult evalImpl(Scope &outer) override {
    return {false, Value{toScope(outer)}};
  }

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  explicit ExprList(Source::Location location,
                    std::vector<std::unique_ptr<Expr>> exprs);

  Scope toScope(Scope &outer);

  using const_iterator = ExprContainer::const_iterator;

  const_iterator begin() const { return exprs_.begin(); }
  const_iterator end() const { return exprs_.end(); }
};

class UnaryOperation : public Expr {
protected:
  EvalResult evalImpl(Scope &s) override {
    return {true, operation_(s, *operand_)};
  }

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  using Function = Value(Scope &, Expr &);

  explicit UnaryOperation(Source::Location location, ASTNodeType opType,
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
  EvalResult evalImpl(Scope &s) override {
    return {true, operation_(s, *operand1_, *operand2_)};
  }

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  using Function = Value(Scope &, Expr &, Expr &);

  explicit BinaryOperation(Source::Location location, ASTNodeType opType,
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
  EvalResult evalImpl(Scope &s) override;

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  explicit CustomOperation(Source::Location location, std::string op,
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

class MutableBinaryOperation : public BinaryOperation {
protected:
  Value *tryMutableEvalImpl(Scope &s) override {
    return mutableOperation_(s, leftOperand(), rightOperand());
  }

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  using MutableFunction = Value *(Scope &, Expr &, Expr &);

  explicit MutableBinaryOperation(Source::Location location, ASTNodeType opType,
                                  Function *operation,
                                  MutableFunction *mutableOperation,
                                  std::unique_ptr<Expr> operand1,
                                  std::unique_ptr<Expr> operand2)
      : BinaryOperation(std::move(location), opType, operation,
                        std::move(operand1), std::move(operand2)),
        mutableOperation_(mutableOperation) {}

private:
  MutableFunction *mutableOperation_;
};

class CharMutableBinaryOperation : public MutableBinaryOperation {
protected:
  Char *tryMutableCharEvalImpl(Scope &s) override {
    return charMutableOperation_(s, leftOperand(), rightOperand());
  }

  void dumpWithIndent(std::ostream &os, int indent) const override;

public:
  using CharMutableFunction = Char *(Scope &, Expr &, Expr &);

  explicit CharMutableBinaryOperation(Source::Location location,
                                      ASTNodeType opType, Function *operation,
                                      MutableFunction *mutableOperation,
                                      CharMutableFunction *charMutableOperation,
                                      std::unique_ptr<Expr> operand1,
                                      std::unique_ptr<Expr> operand2)
      : MutableBinaryOperation(std::move(location), opType, operation,
                               mutableOperation, std::move(operand1),
                               std::move(operand2)),
        charMutableOperation_(charMutableOperation) {}

private:
  CharMutableFunction *charMutableOperation_;
};

namespace detail {
Value buildArgumentsForScopeCall(const Value &lhs, const Value &rhs);
}
} // namespace extense

#endif // _LIB_EXTENSE__AST_HPP
