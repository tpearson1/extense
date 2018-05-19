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

#include <extense/parser.hpp>

static constexpr std::array astNodeEnumStrings{
#define X(a) #a,
    _LIB_EXTENSE__AST_NODE_TYPE_ENUM
#undef X
};

void extense::Expr::displayHeaderWithIndent(std::ostream &os,
                                            int indent) const {
  makeIndent(os, indent);
  os << type_;
  if (location_.valid()) os << " (at " << location_ << ')';
}

[[noreturn]] static void handleThrow(extense::Exception e,
                                     const extense::Source::Location &loc) {
  throw extense::ExceptionWrapper(std::move(e), loc);
}

extense::Proxy extense::Expr::eval(Scope &scope) {
  try {
    return evalImpl(scope);
  } catch (const Exception &e) { handleThrow(e, location_); }
}

extense::Value extense::constEval(Scope &scope, Expr &e) {
  return e.eval(scope).get();
}

std::ostream &extense::operator<<(std::ostream &os, ASTNodeType type) {
  os << astNodeEnumStrings[static_cast<int>(type)];
  return os;
}

class ValueEvaluator : public extense::Proxy::Data {
  extense::Value value_;

public:
  explicit ValueEvaluator(extense::Value value) : value_(std::move(value)) {}

  std::unique_ptr<extense::Proxy::Data> clone() const override {
    return std::make_unique<ValueEvaluator>(value_);
  }

  extense::Value get() const override { return value_; }
  bool isMutable() const override { return false; }
};

extense::Proxy extense::ValueExpr::evalImpl(Scope &s) {
  return Proxy::make<ValueEvaluator>(value_);
}

void extense::ValueExpr::dumpWithIndent(std::ostream &os, int indent) const {
  displayHeaderWithIndent(os, indent);
  os << ": " << LiteralShow{value_} << '\n';
}

extense::Proxy extense::LabelDeclaration::evalImpl(Scope &s) {
  return Proxy::make<ValueEvaluator>(noneValue);
}

void extense::LabelDeclaration::dumpWithIndent(std::ostream &os,
                                               int indent) const {
  displayHeaderWithIndent(os, indent);
  os << ": name '" << name_ << "'\n";
}

extense::Proxy extense::Identifier::evalImpl(Scope &s) {
  class IdentifierEvaluator : public Proxy::Data {
    Scope &scope_;
    std::string &name_;

  public:
    IdentifierEvaluator(Scope &scope, std::string &name)
        : scope_(scope), name_(name) {}

    std::unique_ptr<Proxy::Data> clone() const override {
      return std::make_unique<IdentifierEvaluator>(scope_, name_);
    }

    Value get() const override { return scope_.getIdentifier(name_); }
    bool isMutable() const override { return true; }

    void set(Value v) override {
      scope_.createOrGetIdentifier(name_) = std::move(v);
    }

    void mutate(std::function<void(Value &)> visitor) override {
      auto &ident = scope_.getIdentifier(name_);
      visitor(ident);
    }
  };

  return Proxy::make<IdentifierEvaluator>(s, name_);
}

void extense::Identifier::dumpWithIndent(std::ostream &os, int indent) const {
  displayHeaderWithIndent(os, indent);
  os << ": name '" << name_ << "'\n";
}

void extense::ScopeCall::dumpWithIndent(std::ostream &os, int indent) const {
  displayHeaderWithIndent(os, indent);
  os << ": Scope and argument below\n";
  scope_->dumpWithIndent(os, indent + indentAmount);
  argument_->dumpWithIndent(os, indent + indentAmount);
}

// Used for building an argument from a call using operators such as '.' and
// '::'. Below are examples of how the function behaves.
//
// lhs.x rhs -> x (buildArgumentsForScopeCall (lhs, rhs))
// 7.x 2 -> x (7, 2)
// 5.y (2, 3) -> x (5, 2, 3)
extense::Value extense::detail::buildArgumentsForScopeCall(const Value &lhs,
                                                           const Value &rhs) {
  if (rhs.is<List>()) return Value{List{lhs} + get<List>(rhs)};
  return Value{List{lhs, rhs}};
}

static extense::Scope &getScopeForCall(extense::Value &scope) {
  if (!scope.is<extense::Scope>())
    throw extense::InvalidUnaryOperation{scope, "Can only call Scopes"};
  return extense::get<extense::Scope>(scope);
}

extense::Proxy extense::ScopeCall::evalImpl(Scope &scope) {
  auto argEvaled = constEval(scope, *argument_);

  // Handle special cases
  if (scope_->type() == ASTNodeType::Dot) {
    auto &dotOpExpr = *static_cast<BinaryOperation *>(scope_.get());
    auto lhs = constEval(scope, dotOpExpr.leftOperand());
    auto &toCallExpr = dotOpExpr.rightOperand();
    auto toCallValue = constEval(scope, toCallExpr);
    auto &toCall = getScopeForCall(toCallValue);
    return Proxy::make<ValueEvaluator>(
        toCall(detail::buildArgumentsForScopeCall(lhs, argEvaled)));
  }
  if (scope_->type() == ASTNodeType::ColonColon ||
      scope_->type() == ASTNodeType::SemicolonSemicolon) {
    auto &indexExpr = *static_cast<BinaryOperation *>(scope_.get());
    auto map = constEval(scope, indexExpr.leftOperand());
    auto toCallValue = ops::index(
        map, Value{detail::getIdentifierName(indexExpr.rightOperand())});
    auto &toCall = getScopeForCall(toCallValue);
    return Proxy::make<ValueEvaluator>(
        toCall(detail::buildArgumentsForScopeCall(map, argEvaled)));
  }

  auto toCallValue = constEval(scope, *scope_);
  auto &toCall = getScopeForCall(toCallValue);
  return Proxy::make<ValueEvaluator>(toCall(argEvaled));
}

void extense::MapConstructor::dumpWithIndent(std::ostream &os,
                                             int indent) const {
  displayHeaderWithIndent(os, indent);
  os << ": ";
  if (mappings_.empty()) {
    os << "EMPTY\n";
    return;
  }

  os << "Mappings below\n";
  for (const auto &[key, value] : mappings_) {
    makeIndent(os, indent + indentAmount);
    os << "Mapping:\n";
    key->dumpWithIndent(os, indent + indentAmount * 2);
    value->dumpWithIndent(os, indent + indentAmount * 2);
  }
}

extense::Proxy extense::MapConstructor::evalImpl(Scope &scope) {
  Map s;
  for (auto &[key, value] : mappings_) {
    auto keyValue = constEval(scope, *key);
    s[keyValue] = constEval(scope, *value);
  }
  return Proxy::make<ValueEvaluator>(Value{s});
}

void extense::ListConstructor::dumpWithIndent(std::ostream &os,
                                              int indent) const {
  displayHeaderWithIndent(os, indent);
  os << ": ";
  if (elements_.empty()) {
    os << "EMPTY\n";
    return;
  }

  os << "Elements below\n";
  for (const auto &elem : elements_)
    elem->dumpWithIndent(os, indent + indentAmount);
}

extense::Proxy extense::ListConstructor::evalImpl(Scope &scope) {
  List l;
  std::transform(elements_.begin(), elements_.end(),
                 std::back_inserter(l.value),
                 [&scope](auto &expr) { return constEval(scope, *expr); });
  return Proxy::make<ValueEvaluator>(Value{l});
}

extense::ExprList::ExprList(Source::Location location,
                            std::vector<std::unique_ptr<Expr>> exprs)
    : Expr(std::move(location), ASTNodeType::ExprList),
      exprs_(std::move(exprs)) {
  // Find and add label declarations to their corresponding list
  for (auto it = exprs_.begin(); it != exprs_.end(); it++) {
    if ((*it)->type() != ASTNodeType::LabelDeclaration) continue;
    auto labelDecl = static_cast<LabelDeclaration *>(it->get());
    labels.push_back(
        Label{static_cast<int>(it - exprs_.begin()), labelDecl->name()});
  }
}

extense::Proxy extense::ExprList::evalImpl(Scope &outer) {
  return Proxy::make<ValueEvaluator>(Value{toScope(outer)});
}

void extense::ExprList::dumpWithIndent(std::ostream &os, int indent) const {
  displayHeaderWithIndent(os, indent);
  if (exprs_.empty()) {
    os << ": EMPTY\n";
    return;
  }

  os << ": Expressions below\n";
  for (const auto &expr : exprs_)
    expr->dumpWithIndent(os, indent + indentAmount);
}

extense::Scope extense::ExprList::toScope(Scope &outer) {
  Scope out{[&](Scope &s, const Value &arg) {
              // Inject label variables into scope
              for (auto &label : labels)
                s.createIdentifier(label.name()) = Value{label};

              // Set '$'
              s.createIdentifier("$") = arg;

              if (exprs_.empty()) return noneValue;

              // Evaluate each expression, and return the result of evaluating
              // the last
              std::for_each(exprs_.begin(), exprs_.end() - 1,
                            [&s](auto &expr) { expr->eval(s); });
              return constEval(s, *exprs_.back());
            },
            &outer};
  return out;
}

extense::Proxy extense::UnaryOperation::evalImpl(Scope &scope) {
  return Proxy::make<ValueEvaluator>(operation_(scope, *operand_));
}

void extense::UnaryOperation::dumpWithIndent(std::ostream &os,
                                             int indent) const {
  displayHeaderWithIndent(os, indent);
  os << ": UnaryOperation\n";
  operand_->dumpWithIndent(os, indent + indentAmount);
}

extense::Proxy extense::BinaryOperation::evalImpl(Scope &scope) {
  return Proxy::make<ValueEvaluator>(operation_(scope, *operand1_, *operand2_));
}

void extense::BinaryOperation::dumpWithIndent(std::ostream &os,
                                              int indent) const {
  displayHeaderWithIndent(os, indent);
  os << ": BinaryOperation\n";
  operand1_->dumpWithIndent(os, indent + indentAmount);
  operand2_->dumpWithIndent(os, indent + indentAmount);
}

extense::Proxy extense::CustomOperation::evalImpl(Scope &s) {
  auto opFuncVal = s.getIdentifier(op_);
  auto result =
      get<Scope>(opFuncVal)(constEval(s, *operand1_), constEval(s, *operand2_));
  return Proxy::make<ValueEvaluator>(result);
}

void extense::CustomOperation::dumpWithIndent(std::ostream &os,
                                              int indent) const {
  displayHeaderWithIndent(os, indent);
  os << ": '" << op_ << "'\n";
  operand1_->dumpWithIndent(os, indent + indentAmount);
  operand2_->dumpWithIndent(os, indent + indentAmount);
}

extense::Proxy extense::IndexOperation::evalImpl(Scope &s) {
  class IndexEvaluator : public Proxy::Data {
    Scope &scope_;
    Expr &a, &index;

    Value indexValue() const {
      return (index.type() == ASTNodeType::Semicolon) ?
                 Value{detail::getIdentifierName(index)} :
                 constEval(scope_, index);
    }

  public:
    IndexEvaluator(Scope &scope, Expr &e1, Expr &idx)
        : scope_(scope), a(e1), index(idx) {}

    std::unique_ptr<Proxy::Data> clone() const override {
      return std::make_unique<IndexEvaluator>(scope_, a, index);
    }

    Value get() const override {
      return ops::index(constEval(scope_, a), indexValue());
    }
    bool isMutable() const override { return true; }

    void set(Value v) override {
      a.eval(scope_).mutate([&](Value &m) {
        auto indexVal = indexValue();
        if (m.is<String>()) {
          if (!v.is<Char>()) throw AccessError{};
          if (!indexVal.is<Int>())
            throw InvalidBinaryOperation(m, indexVal, "Unable to index type");
          extense::get<String>(m)[extense::get<Int>(indexVal)] =
              extense::get<Char>(v);
          return;
        }
        ops::mutableIndex(m, indexVal) = std::move(v);
      });
    }
  };

  return Proxy::make<IndexEvaluator>(s, leftOperand(), rightOperand());
}

void extense::IndexOperation::dumpWithIndent(std::ostream &os,
                                             int indent) const {
  displayHeaderWithIndent(os, indent);
  os << ": IndexOperation (" << type() << ")\n";
  leftOperand().dumpWithIndent(os, indent + indentAmount);
  rightOperand().dumpWithIndent(os, indent + indentAmount);
}
