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
#include <extense/value.hpp>

const extense::Char &extense::String::operator[](Int i) const {
  if (i >= size() || i.value < 0)
    throw InvalidBinaryOperation::Create<String, Int>(
        "Out of bounds string access");
  // Can reinterpret_cast because the memory layout of Char and char is the same
  return reinterpret_cast<const Char &>(value[i.value]);
}

bool extense::detail::MapCompare::operator()(const MapKeyType &lhs,
                                             const MapKeyType &rhs) const {
  // It is necessary to mostly reimplement variant's operator< because we do not
  // want to expose arbitrary operator< definitions to the end user and the
  // language
  const auto &v = lhs.internalVariant(), &w = rhs.internalVariant();
  if (w.valueless_by_exception()) return false;
  if (v.valueless_by_exception()) return false;
  if (v.index() < w.index()) return true;
  if (v.index() > w.index()) return false;

  // Instead of just using operator< on the contained types, we need to do some
  // manipulation first to make it play nice with our define value types
  return std::visit(
      [&w](const auto &l) -> bool {
        using VT = std::decay_t<decltype(l)>;
        // The second variant is now guaranteed to share the same type as the
        // first
        const auto &r = std::get<VT>(w);

        if constexpr (std::is_same_v<VT, None>)
          return false;
        else
          return l.value < r.value;
      },
      v);
}

extense::Map::Map(
    detail::Wrap<std::initializer_list<ValueType::value_type>> kvps)
    : Base{ValueType(std::move(kvps).value)} {}

extense::Map::KeyType extense::Map::constrainToKeyType(const Value &v) {
  return constrain<Map::KeyType>(v.flatten());
}

extense::Map::KeyType extense::Map::constrainToKeyType(const FlatValue &v) {
  return constrain<Map::KeyType>(v);
}

const extense::Value &extense::Map::at(const KeyType &i) const {
  try {
    return value.at(i);
  } catch (std::out_of_range &) {
    throw InvalidBinaryOperation{"Map", "Map's key type",
                                 "Element not present in map"};
  }
}

extense::List::List(detail::Wrap<std::initializer_list<Value>> values)
    : Base{ValueType(std::move(values).value)} {}

const extense::Value &extense::List::operator[](extense::Int i) const {
  if (i.value >= static_cast<Int::ValueType>(value.size()) || i.value < 0)
    throw InvalidBinaryOperation::Create<List, Int>(
        "Out of bounds array access");
  return value[i.value];
}

const extense::Value &extense::List::at(extense::Int i) const {
  return (*this)[i];
}

extense::List extense::List::operator[](const extense::List &i) const {
  if (i.value.size() != 2) {
    throw InvalidBinaryOperation::Create<List, List>(
        "Indexing using list with size not equal to 2");
  }

  if (!i.value[0].is<Int>() || !i.value[1].is<Int>()) {
    throw InvalidBinaryOperation::Create<List, List>(
        "Bounds specified in indexing list must be Ints");
  }

  auto &lowerBound = get<Int>(i.value[0]).value;
  auto &upperBound = get<Int>(i.value[1]).value;

  if (upperBound < lowerBound) {
    throw InvalidBinaryOperation::Create<List, List>(
        "Indexing using an list with second element less than the first");
  }

  if (lowerBound < 0) {
    throw InvalidBinaryOperation::Create<List, List>(
        "First element in indexing list is less than 0");
  }

  if (upperBound >= static_cast<Int::ValueType>(value.size()))
    throw InvalidBinaryOperation::Create<List, List>(
        "Out of bounds list access");

  List sublist;
  const auto &begin = std::begin(value);
  std::copy(begin + lowerBound, begin + upperBound,
            std::back_inserter(sublist.value));
  return sublist;
}

extense::Value extense::List::at(const Value &i) const {
  if (i.is<Int>())
    return Value{at(get<Int>(i))};
  else if (i.is<List>())
    return Value{at(get<List>(i))};
  throw InvalidBinaryOperation("List", i.typeAsString(),
                               "Invalid type for indexer");
}

extense::Value extense::Scope::call(const Value &v) {
  auto result = func(*this, v);
  clearIdentifiers();
  return result;
}

const extense::Value &
extense::Scope::getIdentifier(const std::string &name) const {
  auto *v = findIdentifier(name);
  if (!v) throw IdentifierError{name};
  return *v;
}

const extense::Value *
extense::Scope::findIdentifier(const std::string &name) const {
  for (auto *s = this; s != nullptr; s = s->outer_) {
    if (auto it = s->identifiers.find(name); it != s->identifiers.end())
      return &it->second;
  }

  return nullptr;
}

extense::Value &extense::Scope::createIdentifier(const std::string &name) {
  return identifiers[name];
}

extense::Value &extense::Scope::createOrGetIdentifier(const std::string &name) {
  auto *ident = findIdentifier(name);
  if (ident) return *ident;
  return identifiers[name];
}

void extense::detail::throwOperatorOverloadError() {
  throw OperatorOverloadError{};
}

#define USER_OBJECT_BINARY_FUNCTION(name)                                      \
  extense::Value extense::UserObject::Data::name(const Value &) {              \
    extense::detail::throwOperatorOverloadError();                             \
  }
#define USER_OBJECT_UNARY_FUNCTION(name)                                       \
  extense::Value extense::UserObject::Data::name() {                           \
    extense::detail::throwOperatorOverloadError();                             \
  }

USER_OBJECT_BINARY_FUNCTION(add)
USER_OBJECT_BINARY_FUNCTION(addEquals)
USER_OBJECT_UNARY_FUNCTION(unaryPlus)

USER_OBJECT_BINARY_FUNCTION(sub)
USER_OBJECT_BINARY_FUNCTION(subEquals)
USER_OBJECT_UNARY_FUNCTION(unaryMinus)

USER_OBJECT_BINARY_FUNCTION(mul)
USER_OBJECT_BINARY_FUNCTION(mulEquals)

USER_OBJECT_BINARY_FUNCTION(div)
USER_OBJECT_BINARY_FUNCTION(divEquals)

USER_OBJECT_BINARY_FUNCTION(mod)
USER_OBJECT_BINARY_FUNCTION(modEquals)

USER_OBJECT_BINARY_FUNCTION(floorDiv)
USER_OBJECT_BINARY_FUNCTION(floorDivEquals)

USER_OBJECT_BINARY_FUNCTION(pow)
USER_OBJECT_BINARY_FUNCTION(powEquals)

USER_OBJECT_BINARY_FUNCTION(dotDot)

USER_OBJECT_BINARY_FUNCTION(bitAnd)
USER_OBJECT_BINARY_FUNCTION(bitAndEquals)

USER_OBJECT_BINARY_FUNCTION(bitOr)
USER_OBJECT_BINARY_FUNCTION(bitOrEquals)

USER_OBJECT_BINARY_FUNCTION(bitXor)
USER_OBJECT_BINARY_FUNCTION(bitXorEquals)

USER_OBJECT_UNARY_FUNCTION(bitNot)

USER_OBJECT_BINARY_FUNCTION(bitLShift)
USER_OBJECT_BINARY_FUNCTION(bitLShiftEquals)

USER_OBJECT_BINARY_FUNCTION(bitRShift)
USER_OBJECT_BINARY_FUNCTION(bitRShiftEquals)

USER_OBJECT_BINARY_FUNCTION(logicalAnd)
USER_OBJECT_BINARY_FUNCTION(logicalOr)

USER_OBJECT_BINARY_FUNCTION(lessThan)
USER_OBJECT_BINARY_FUNCTION(lessEquals)
USER_OBJECT_BINARY_FUNCTION(greaterThan)
USER_OBJECT_BINARY_FUNCTION(greaterEquals)

extense::Bool extense::UserObject::Data::equal(const Value &) const {
  extense::detail::throwOperatorOverloadError();
}
extense::Bool extense::UserObject::Data::notEqual(const Value &) const {
  extense::detail::throwOperatorOverloadError();
}

#undef USER_OBJECT_UNARY_FUNCTION
#undef USER_OBJECT_BINARY_FUNCTION

#define USER_OBJECT_BINARY_DELEGATOR(name)                                     \
  extense::Value extense::UserObject::name(const Value &v) {                   \
    return data_->name(v);                                                     \
  }
#define USER_OBJECT_UNARY_DELEGATOR(name)                                      \
  extense::Value extense::UserObject::name() { return data_->name(); }

USER_OBJECT_BINARY_DELEGATOR(add)
USER_OBJECT_BINARY_DELEGATOR(addEquals)
USER_OBJECT_UNARY_DELEGATOR(unaryPlus)

USER_OBJECT_BINARY_DELEGATOR(sub)
USER_OBJECT_BINARY_DELEGATOR(subEquals)
USER_OBJECT_UNARY_DELEGATOR(unaryMinus)

USER_OBJECT_BINARY_DELEGATOR(mul)
USER_OBJECT_BINARY_DELEGATOR(mulEquals)

USER_OBJECT_BINARY_DELEGATOR(div)
USER_OBJECT_BINARY_DELEGATOR(divEquals)

USER_OBJECT_BINARY_DELEGATOR(mod)
USER_OBJECT_BINARY_DELEGATOR(modEquals)

USER_OBJECT_BINARY_DELEGATOR(floorDiv)
USER_OBJECT_BINARY_DELEGATOR(floorDivEquals)

USER_OBJECT_BINARY_DELEGATOR(pow)
USER_OBJECT_BINARY_DELEGATOR(powEquals)

USER_OBJECT_BINARY_DELEGATOR(dotDot)

USER_OBJECT_BINARY_DELEGATOR(bitAnd)
USER_OBJECT_BINARY_DELEGATOR(bitAndEquals)

USER_OBJECT_BINARY_DELEGATOR(bitOr)
USER_OBJECT_BINARY_DELEGATOR(bitOrEquals)

USER_OBJECT_BINARY_DELEGATOR(bitXor)
USER_OBJECT_BINARY_DELEGATOR(bitXorEquals)

USER_OBJECT_UNARY_DELEGATOR(bitNot)

USER_OBJECT_BINARY_DELEGATOR(bitLShift)
USER_OBJECT_BINARY_DELEGATOR(bitLShiftEquals)

USER_OBJECT_BINARY_DELEGATOR(bitRShift)
USER_OBJECT_BINARY_DELEGATOR(bitRShiftEquals)

USER_OBJECT_BINARY_DELEGATOR(logicalAnd)
USER_OBJECT_BINARY_DELEGATOR(logicalOr)

USER_OBJECT_BINARY_DELEGATOR(lessThan)
USER_OBJECT_BINARY_DELEGATOR(lessEquals)
USER_OBJECT_BINARY_DELEGATOR(greaterThan)
USER_OBJECT_BINARY_DELEGATOR(greaterEquals)

extense::Bool extense::UserObject::equal(const Value &v) const {
  return data_->equal(v);
}
extense::Bool extense::UserObject::notEqual(const Value &v) const {
  return data_->notEqual(v);
}

#undef USER_OBJECT_UNARY_DELEGATOR
#undef USER_OBJECT_BINARY_DELEGATOR

std::ostream &extense::operator<<(std::ostream &os, const List &v) {
  auto &list = v.value;
  os << '(';
  if (!list.empty()) {
    auto it = std::begin(list);
    auto lastElem = --std::end(list);
    for (; it != lastElem; it++) os << extense::LiteralShow{*it} << ',';
    os << extense::LiteralShow{*lastElem};
    if (list.size() == 1) {
      // Show that it is a list by adding a trailing comma
      os << ',';
    }
  }
  os << ')';
  return os;
}

std::ostream &extense::operator<<(std::ostream &os, const Map &v) {
  auto &map = v.value;
  os << (map.empty() ? "{" : "{\n");
  for (const auto &[k, v] : map)
    os << extense::LiteralShow{k} << " -> " << extense::LiteralShow{v} << '\n';
  os << '}';
  return os;
}
