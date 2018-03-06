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

static constexpr std::array astNodeEnumStrings{
#define X(a) #a,
    _LIB_EXTENSE__AST_NODE_TYPE_ENUM
#undef X
};

std::ostream &extense::operator<<(std::ostream &os, ASTNodeType type) {
  os << astNodeEnumStrings[static_cast<int>(type)];
  return os;
}

void extense::ValueExpr::dumpWithIndent(std::ostream &os, int indent) const {
  makeIndent(os, indent);
  os << "ValueExpr: " << LiteralShow{value_} << '\n';
}

extense::Expr::EvalResult extense::ValueExpr::eval(Scope &) {
  if (value_.is<Scope>()) return {false, get<Scope>(value_)()};
  return {false, value_};
}

void extense::LabelDeclaration::dumpWithIndent(std::ostream &os,
                                               int indent) const {
  makeIndent(os, indent);
  os << "LabelDeclaration: name '" << name_ << "'\n";
}

void extense::Identifier::dumpWithIndent(std::ostream &os, int indent) const {
  makeIndent(os, indent);
  os << "Identifier: name '" << name_ << "'\n";
}

extense::Expr::EvalResult extense::Identifier::eval(Scope &s) {
  return {true, s.getIdentifier(name_)};
}

extense::Value *extense::Identifier::tryMutableEval(Scope &s) {
  return &s.createOrGetIdentifier(name_);
}

void extense::ScopeCall::dumpWithIndent(std::ostream &os, int indent) const {
  makeIndent(os, indent);
  os << "ScopeCall: Scope and argument below\n";
  scope_->dumpWithIndent(os, indent + indentAmount);
  argument_->dumpWithIndent(os, indent + indentAmount);
}

extense::Expr::EvalResult extense::ScopeCall::eval(Scope &scope) {
  auto scopeEvaluated = scope_->eval(scope).value;
  if (!scopeEvaluated.is<Scope>()) {
    // TODO: Custom exception
    throw std::runtime_error{"Can only call Scopes"};
  }

  return {true, get<Scope>(scopeEvaluated)(argument_->eval(scope).value)};
}

void extense::MapConstructor::dumpWithIndent(std::ostream &os,
                                             int indent) const {
  makeIndent(os, indent);
  os << "MapConstructor: ";
  if (mappings_.empty()) {
    os << "EMPTY\n";
    return;
  }

  os << "Mappings below\n";
  for (const auto & [ key, value ] : mappings_) {
    makeIndent(os, indent + indentAmount);
    os << "Mapping\n";
    key->dumpWithIndent(os, indent + indentAmount * 2);
    value->dumpWithIndent(os, indent + indentAmount * 2);
  }
}

extense::Expr::EvalResult extense::MapConstructor::eval(Scope &scope) {
  Map s;
  // TODO: Catch exception which may be thrown by constrain and throw a custom
  // one
  for (auto & [ key, value ] : mappings_) {
    auto keyValue = key->eval(scope).value;
    s[keyValue] = value->eval(scope).value;
  }
  return {false, Value{s}};
}

void extense::ListConstructor::dumpWithIndent(std::ostream &os,
                                              int indent) const {
  makeIndent(os, indent);
  os << "ListConstructor: ";
  if (elements_.empty()) {
    os << "EMPTY\n";
    return;
  }

  os << "Elements below\n";
  for (const auto &elem : elements_)
    elem->dumpWithIndent(os, indent + indentAmount);
}

extense::Expr::EvalResult extense::ListConstructor::eval(Scope &scope) {
  List l;
  std::transform(elements_.begin(), elements_.end(),
                 std::back_inserter(l.value),
                 [&scope](auto &expr) { return expr->eval(scope).value; });
  return {false, Value{l}};
}

extense::ExprList::ExprList(std::vector<std::unique_ptr<Expr>> exprs)
    : Expr(ASTNodeType::ExprList), exprs_(std::move(exprs)) {
  // Find and add label declarations to their corresponding list
  for (auto it = exprs_.begin(); it != exprs_.end(); it++) {
    if ((*it)->type() != ASTNodeType::LabelDeclaration) continue;
    auto labelDecl = static_cast<LabelDeclaration *>(it->get());
    labels.push_back(
        Label{static_cast<int>(it - exprs_.begin()), labelDecl->name()});
  }
}

void extense::ExprList::dumpWithIndent(std::ostream &os, int indent) const {
  makeIndent(os, indent);
  os << "ExprList: Expressions below\n";
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
              return exprs_.back()->eval(s).value;
            },
            &outer};
  return out;
}
