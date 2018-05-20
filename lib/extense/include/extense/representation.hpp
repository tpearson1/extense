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

#ifndef _LIB_EXTENSE__REPRESENTATION_HPP
#define _LIB_EXTENSE__REPRESENTATION_HPP

#include <extense/value.hpp>

namespace extense {
template <typename T>
auto toValueType(const T &val) {
  if constexpr (Int::canRepresent<T>) return Int{val};
  if constexpr (Float::canRepresent<T>) return Float{val};
  if constexpr (Bool::canRepresent<T>) return Bool{val};
  if constexpr (Char::canRepresent<T>) return Char{val};
  if constexpr (String::canRepresent<T>) return String{val};

  throw InvalidConversion{"Internal representation", "ValueType", false};
}

template <typename T>
auto toValue(const T &val) {
  return Value{toValueType(val)};
}

template <typename Result, typename VT>
Result fromValueType(const VT &val) {
  if constexpr (std::is_same_v<VT, Int> && Int::canRepresent<Result>)
    return val.template to<Result>();
  if constexpr (std::is_same_v<VT, Float> && Float::canRepresent<Result>)
    return val.template to<Result>();
  if constexpr (std::is_same_v<VT, Bool> && Bool::canRepresent<Result>)
    return val.template to<Result>();
  if constexpr (std::is_same_v<VT, Char> && Char::canRepresent<Result>)
    return val.template to<Result>();
  if constexpr (std::is_same_v<VT, String> && String::canRepresent<Result>)
    return val.template to<Result>();

  throw InvalidConversion{"ValueType", "Internal representation", false};
}

template <typename Result>
Result fromValue(const Value &v) {
  return visit([](const auto &v) { return fromValueType<Result>(v); }, v);
}

template <typename Rep>
class RepReadProxy : public Proxy::Data {
  const Rep &rep_;

public:
  RepReadProxy(const Rep &rep) : rep_(rep) {}

  Value get() const override { return toValue(rep_); }
};

template <typename Rep>
class RepWriteProxy : public Proxy::Data {
  Rep &rep_;

public:
  RepWriteProxy(Rep &rep) : rep_(rep) {}

  void set(Value v) override { rep_ = fromValue<Rep>(v); }
};
} // namespace extense

#endif // _LIB_EXTENSE__REPRESENTATION_HPP
