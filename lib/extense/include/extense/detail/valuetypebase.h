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

#ifndef _LIB_EXTENSE_DETAIL__VALUE_TYPE_BASE_H
#define _LIB_EXTENSE_DETAIL__VALUE_TYPE_BASE_H

#include <type_traits>
#include <utility>

namespace extense {
namespace detail {
template <typename From, typename To>
struct Convert {
  struct Cannot {};
};

template <typename From, typename To, typename = void>
struct Convertible : std::true_type {};

template <typename From>
struct Convertible<From, From> : std::true_type {};

template <typename From, typename To>
struct Convertible<From, To, std::void_t<typename Convert<From, To>::Cannot>>
    : std::false_type {};

template <typename From, typename To, typename = void>
struct ImplicitlyConvertible : std::false_type {};

template <typename From>
struct ImplicitlyConvertible<From, From> : std::true_type {};

template <typename From, typename To>
struct ImplicitlyConvertible<
    From, To, std::void_t<typename Convert<From, To>::Implicitly>>
    : std::true_type {};
} // namespace detail

template <typename From, typename To>
inline constexpr bool convertible =
    detail::Convertible<From, To>::value || std::is_same_v<From, To>;

template <typename From, typename To>
inline constexpr bool implicitlyConvertible =
    detail::ImplicitlyConvertible<From, To>::value || std::is_same_v<From, To>;

// Converts one value type into another, which may be either implicit or
// explicit
template <typename From, typename To>
To convert(From &&from) {
  static_assert(convertible<From, To>,
                "Cannot convert between the specified types");
  if constexpr (std::is_same_v<From, To>)
    return std::forward(from);
  else
    return detail::Convert<From, To>{}(std::forward(from));
}

template <typename From, typename To>
To convert(const From &from) {
  static_assert(convertible<From, To>,
                "Cannot convert between the specified types");
  if constexpr (std::is_same_v<From, To>)
    return from;
  else
    return detail::Convert<From, To>{}(from);
}

namespace detail {
template <typename Derived, typename VT>
struct ValueTypeBase {
  using ValueType = VT;
  ValueType value;

  explicit ValueTypeBase(ValueType v) : value(std::move(v)) {}

  template <typename T,
            std::enable_if_t<implicitlyConvertible<T, Derived>> * = nullptr>
  ValueTypeBase(const T &v) {
    *static_cast<Derived *>(this) = convert<T, Derived>(std::move(v));
  }

  template <typename T,
            std::enable_if_t<convertible<T, Derived> &&
                             !implicitlyConvertible<T, Derived>> * = nullptr>
  explicit ValueTypeBase(const T &v) {
    *static_cast<Derived *>(this) = convert<T, Derived>(std::move(v));
  }

  bool operator==(ValueTypeBase other) const { return value == other.value; }
  bool operator!=(ValueTypeBase other) const { return value != other.value; }
  bool operator<=(ValueTypeBase other) const { return value <= other.value; }
  bool operator>=(ValueTypeBase other) const { return value >= other.value; }
  bool operator<(ValueTypeBase other) const { return value < other.value; }
  bool operator>(ValueTypeBase other) const { return value > other.value; }

protected:
  using Base = ValueTypeBase;
};
} // namespace detail
} // namespace extense

#endif /* _LIB_EXTENSE_DETAIL__VALUE_TYPE_BASE_H */
