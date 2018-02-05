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

void extense::LabelDeclaration::dumpWithIndent(std::ostream &os,
                                               int indent) const {
  makeIndent(os, indent);
  os << "LabelDeclaration: name '" << name_ << "'\n";
}

void extense::Identifier::dumpWithIndent(std::ostream &os, int indent) const {
  makeIndent(os, indent);
  os << "Identifier: name '" << name_ << "'\n";
}

void extense::ScopeCall::dumpWithIndent(std::ostream &os, int indent) const {
  makeIndent(os, indent);
  os << "ScopeCall: Scope and argument below\n";
  scope_->dumpWithIndent(os, indent + indentAmount);
  argument_->dumpWithIndent(os, indent + indentAmount);
}

extense::Value extense::ScopeCall::eval(Scope &scope) {
  auto scopeEvaluated = scope_->eval(scope);
  if (!scopeEvaluated.is<Scope>()) {
    // TODO: Custom exception
    throw std::runtime_error{"Cannot only call Scopes"};
  }

  return get<Scope>(scopeEvaluated)(argument_->eval(scope));
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

extense::Value extense::MapConstructor::eval(Scope &scope) {
  Map s;
  // TODO: Catch exception which may be thrown by constrain and throw a custom
  // one
  for (auto & [ key, value ] : mappings_) {
    auto keyValue = key->eval(scope);
    s[keyValue] = value->eval(scope);
  }
  return Value{s};
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

extense::Value extense::ListConstructor::eval(Scope &scope) {
  List l;
  std::transform(elements_.begin(), elements_.end(),
                 std::back_inserter(l.value),
                 [&scope](auto &expr) { return expr->eval(scope); });
  return Value{l};
}

void extense::ExprList::dumpWithIndent(std::ostream &os, int indent) const {
  makeIndent(os, indent);
  os << "ExprList: Expressions below\n";
  for (const auto &expr : exprs_)
    expr->dumpWithIndent(os, indent + indentAmount);
}

extense::Value extense::ExprList::eval(Scope &scope) {
  std::for_each(exprs_.begin(), exprs_.end() - 1,
                [&scope](auto &expr) { expr->eval(scope); });
  return exprs_.back()->eval(scope);
}
