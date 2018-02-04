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

#include <extense/value.hpp>

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
    throw InvalidOperation{"Map", "Map's key type",
                           "Element not present in map"};
  }
}

extense::List::List(detail::Wrap<std::initializer_list<Value>> values)
    : Base{ValueType(std::move(values).value)} {}

const extense::Value &extense::List::operator[](extense::Int i) const {
  if (i.value >= static_cast<Int::ValueType>(value.size()) || i.value < 0)
    throw InvalidOperation::Create<List, Int>("Out of bounds array access");
  return value[i.value];
}

const extense::Value &extense::List::at(extense::Int i) const {
  return (*this)[i];
}

extense::List extense::List::operator[](const extense::List &i) const {
  if (i.value.size() != 2) {
    throw InvalidOperation::Create<List, List>(
        "Indexing using list with size not equal to 2");
  }

  if (!i.value[0].is<Int>() || !i.value[1].is<Int>()) {
    throw InvalidOperation::Create<List, List>(
        "Bounds specified in indexing list must be Ints");
  }

  auto &lowerBound = get<Int>(i.value[0]).value;
  auto &upperBound = get<Int>(i.value[1]).value;

  if (upperBound < lowerBound) {
    throw InvalidOperation::Create<List, List>(
        "Indexing using an list with second element less than the first");
  }

  if (lowerBound < 0) {
    throw InvalidOperation::Create<List, List>(
        "First element in indexing list is less than 0");
  }

  if (upperBound >= static_cast<Int::ValueType>(value.size()))
    throw InvalidOperation::Create<List, List>("Out of bounds list access");

  List sublist;
  const auto &begin = std::begin(value);
  std::copy(begin + lowerBound, begin + upperBound,
            std::back_inserter(sublist.value));
  return sublist;
}

extense::Value extense::Scope::operator()() { return (*this)(noneValue); }

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
  for (const auto & [ k, v ] : map)
    os << extense::LiteralShow{k} << " -> " << extense::LiteralShow{v} << '\n';
  os << '}';
  return os;
}
