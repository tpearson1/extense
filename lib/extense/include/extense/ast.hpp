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

#include <extense/value.hpp>

namespace extense {
#define _LIB_EXTENSE__AST_NODE_TYPE_ENUM                                       \
  X(CustomOperator)                                                            \
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
  X(ColonColon)                                                                \
  X(Is)                                                                        \
  X(DotDot)                                                                    \
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

class Expr {
private:
  ASTNodeType type_;

protected:
  void setType(ASTNodeType type) { type_ = type; }

public:
  explicit Expr(ASTNodeType type) : type_(type) {}

  ASTNodeType type() const { return type_; }

  void dump(std::ostream &os) { dumpWithIndent(os, 0); }
  virtual void dumpWithIndent(std::ostream &os, int indent) const = 0;

  struct EvalResult {
    bool isMutable = false;
    Value value;
  };

  virtual EvalResult eval(Scope &scope) = 0;
  virtual Value *tryMutableEval(Scope &) { return nullptr; }

  virtual ~Expr() {}
};

inline Value constEval(Scope &scope, Expr &e) { return e.eval(scope).value; }

template <typename OpFunc>
auto mutableEval(Scope &scope, Expr &a, OpFunc f) {
  auto *lhs = a.tryMutableEval(scope);
  if (lhs) return f(*lhs);

  auto[isMutable, value] = a.eval(scope);
  // TODO: Custom exception
  if (!isMutable || !value.is<Reference>())
    throw std::runtime_error{"Expected mutable expression"};
  return f(value);
}

class ValueExpr : public Expr {
  Value value_;

public:
  explicit ValueExpr(Value v)
      : Expr(ASTNodeType::ValueExpr), value_(std::move(v)) {}

  void dumpWithIndent(std::ostream &os, int indent) const override;

  const Value &value() const { return value_; }
  Value &value() { return value_; }

  EvalResult eval(Scope &) override;
};

class LabelDeclaration : public Expr {
  std::string name_;

public:
  explicit LabelDeclaration(std::string name)
      : Expr(ASTNodeType::LabelDeclaration), name_(std::move(name)) {}

  void dumpWithIndent(std::ostream &os, int indent) const override;

  const std::string &name() const { return name_; }
  void rename(std::string name) { name_ = std::move(name); }

  EvalResult eval(Scope &) override { return {false, noneValue}; }
};

class Identifier : public Expr {
  std::string name_;

public:
  explicit Identifier(std::string name)
      : Expr(ASTNodeType::Identifier), name_(std::move(name)) {}

  void dumpWithIndent(std::ostream &os, int indent) const override;

  const std::string &name() const { return name_; }
  void rename(std::string name) { name_ = std::move(name); }

  EvalResult eval(Scope &s) override;
  Value *tryMutableEval(Scope &s) override;
};

class ScopeCall : public Expr {
  std::unique_ptr<Expr> scope_;
  std::unique_ptr<Expr> argument_;

public:
  explicit ScopeCall(std::unique_ptr<Expr> scope,
                     std::unique_ptr<Expr> argument)
      : Expr(ASTNodeType::ScopeCall), scope_(std::move(scope)),
        argument_(std::move(argument)) {}

  void dumpWithIndent(std::ostream &os, int indent) const override;

  EvalResult eval(Scope &) override;
};

struct ParsedMapping {
  std::unique_ptr<Expr> key, value;
};

class MapConstructor : public Expr {
  std::vector<ParsedMapping> mappings_;

public:
  explicit MapConstructor(std::vector<ParsedMapping> mappings)
      : Expr(ASTNodeType::MapConstructor), mappings_(std::move(mappings)) {}

  void dumpWithIndent(std::ostream &os, int indent) const override;

  // Returns a Map created from the evaluated mappings
  EvalResult eval(Scope &) override;
};

class ListConstructor : public Expr {
  std::vector<std::unique_ptr<Expr>> elements_;

public:
  explicit ListConstructor(std::vector<std::unique_ptr<Expr>> elements)
      : Expr(ASTNodeType::ListConstructor), elements_(std::move(elements)) {}

  void dumpWithIndent(std::ostream &os, int indent) const override;

  // Returns a List constructed from the evaluated elements
  EvalResult eval(Scope &) override;
};

class ExprList : public Expr {
  using ExprContainer = std::vector<std::unique_ptr<Expr>>;
  ExprContainer exprs_;
  std::vector<Label> labels;

public:
  explicit ExprList(std::vector<std::unique_ptr<Expr>> exprs);

  void dumpWithIndent(std::ostream &os, int indent) const override;

  Scope toScope(Scope &outer);
  EvalResult eval(Scope &outer) override {
    return {false, Value{toScope(outer)}};
  }

  using const_iterator = ExprContainer::const_iterator;

  const_iterator begin() const { return exprs_.begin(); }
  const_iterator end() const { return exprs_.end(); }
};

class UnaryOperation : public Expr {
public:
  using Function = Value(Scope &, Expr &);

  explicit UnaryOperation(ASTNodeType opType, Function *operation,
                          std::unique_ptr<Expr> operand)
      : Expr(opType), operation_(operation), operand_(std::move(operand)) {}

  void dumpWithIndent(std::ostream &os, int indent) const override {
    makeIndent(os, indent);
    os << "UnaryOperation: type '" << type() << "'\n";
    operand_->dumpWithIndent(os, indent + indentAmount);
  }

  EvalResult eval(Scope &s) override {
    return {true, operation_(s, *operand_)};
  }

private:
  Function *operation_;
  std::unique_ptr<Expr> operand_;
};

class BinaryOperation : public Expr {
public:
  using Function = Value(Scope &, Expr &, Expr &);

  explicit BinaryOperation(ASTNodeType opType, Function *operation,
                           std::unique_ptr<Expr> operand1,
                           std::unique_ptr<Expr> operand2)
      : Expr(opType), operation_(operation), operand1_(std::move(operand1)),
        operand2_(std::move(operand2)) {}

  void dumpWithIndent(std::ostream &os, int indent) const override {
    makeIndent(os, indent);
    os << "BinaryOperation: type '" << type() << "'\n";
    operand1_->dumpWithIndent(os, indent + indentAmount);
    operand2_->dumpWithIndent(os, indent + indentAmount);
  }

  EvalResult eval(Scope &s) override {
    return {true, operation_(s, *operand1_, *operand2_)};
  }

private:
  Function *operation_;
  std::unique_ptr<Expr> operand1_, operand2_;
};

class PossiblyMutableBinaryOperation : public Expr {
public:
  using Function = Value(Scope &, Expr &, Expr &);
  using MutableFunction = Value *(Scope &, Expr &, Expr &);

  explicit PossiblyMutableBinaryOperation(ASTNodeType opType,
                                          Function *operation,
                                          MutableFunction *mutableOperation,
                                          std::unique_ptr<Expr> operand1,
                                          std::unique_ptr<Expr> operand2)
      : Expr(opType), operation_(operation),
        mutableOperation_(mutableOperation), operand1_(std::move(operand1)),
        operand2_(std::move(operand2)) {}

  void dumpWithIndent(std::ostream &os, int indent) const override {
    makeIndent(os, indent);
    os << "BinaryOperation: type '" << type() << "'\n";
    operand1_->dumpWithIndent(os, indent + indentAmount);
    operand2_->dumpWithIndent(os, indent + indentAmount);
  }

  EvalResult eval(Scope &s) override {
    return {true, operation_(s, *operand1_, *operand2_)};
  }

  Value *tryMutableEval(Scope &s) override {
    return mutableOperation_(s, *operand1_, *operand2_);
  }

private:
  Function *operation_;
  MutableFunction *mutableOperation_;
  std::unique_ptr<Expr> operand1_, operand2_;
};
} // namespace extense

#endif // _LIB_EXTENSE__AST_HPP
