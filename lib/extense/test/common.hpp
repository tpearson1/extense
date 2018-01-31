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

#ifndef _LIB_EXTENSE_TEST__COMMON_HPP
#define _LIB_EXTENSE_TEST__COMMON_HPP

#include <variant>

template <typename T, typename TVariant,
          typename EqFunc = bool (*)(const T &, const T &)>
inline bool variantEquals(const TVariant &v, const T &t,
                          EqFunc eq = [](auto a, auto b) { return a == b; }) {
  return std::holds_alternative<T>(v) && eq(std::get<T>(v), t);
}

#endif /* _LIB_EXTENSE_TEST__COMMON_HPP */
