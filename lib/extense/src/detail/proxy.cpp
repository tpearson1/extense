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

[[noreturn]] static void throwAccessError() { throw extense::AccessError{}; }

// By default the functions act as if they cannot be used
std::unique_ptr<extense::Proxy::Data> extense::Proxy::Data::clone() const {
  throw CopyError{};
}
extense::Value extense::Proxy::Data::get() const { throwAccessError(); }
void extense::Proxy::Data::set(Value) { throwAccessError(); }

// By default, use set and get to emulate direct mutation. If the value is not
// mutable, will try and get a reference from get.
void extense::Proxy::Data::mutate(std::function<void(Value &)> visitor) {
  auto v = get();
  visitor(v);
  if (isMutable()) set(v);
}

// By default not considered mutable
bool extense::Proxy::Data::isMutable() const { return false; }

// Delegators from Proxy to Proxy::Data
extense::Value extense::Proxy::get() const { return data_->get(); }
void extense::Proxy::set(Value v) { data_->set(v); }
void extense::Proxy::mutate(std::function<void(Value &)> visitor) {
  data_->mutate(visitor);
}

std::ostream &extense::operator<<(std::ostream &os, const Proxy &p) {
  os << p.get();
  return os;
}
