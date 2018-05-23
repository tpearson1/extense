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

std::string extense::InvalidConversion::buildError() const {
  std::ostringstream os;
  os << "Invalid " << (implicit ? "implicit" : "explicit")
     << " conversion from '" + fromT + "' to '" + toT + '\'';
  return os.str();
}

void extense::Value::constructFromFlatValue(FlatValue fv) {
  visit([this](auto &v) { *this = Value{v}; }, fv);
}

std::string extense::Value::typeAsString(bool displayReference) const {
  std::string result;
  if (displayReference && is<Reference>()) result = "Reference to ";
  return result + visit(
                      [](auto &&arg) -> std::string {
                        using VT = std::decay_t<decltype(arg)>;
                        return std::string(extense::typeAsString<VT>);
                      },
                      *this);
}

std::ostream &extense::operator<<(std::ostream &os, const Reference &v) {
  os << *v;
  return os;
}

std::ostream &extense::operator<<(std::ostream &os, const Value &v) {
  std::visit([&os](const auto &arg) { os << arg; }, v.data);
  return os;
}
