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

#ifndef _LIB_EXTENSE_DETAIL_PROXY_H
#define _LIB_EXTENSE_DETAIL_PROXY_H

#include <functional>
#include <memory>

namespace extense {
class Value;

/*
 * Facilitates storing values differently than that of a Value. This makes it
 * possible to cleanly expose C++ variables to the language without being forced
 * to use the given ValueTypes.
 */
class Proxy final {
public:
  class Data {
  public:
    virtual std::unique_ptr<Data> clone() const;
    virtual ~Data() {}

    virtual Value get() const;

    virtual bool isMutable() const;
    virtual void set(Value v);
    virtual void mutate(std::function<void(Value &)> visitor);
  };

private:
  std::unique_ptr<Data> data_;

public:
  explicit Proxy(std::unique_ptr<Data> data) : data_(std::move(data)) {}

  const Data &data() const { return *data_; }
  Data &data() { return *data_; }

  template <typename Derived, typename... Args>
  static Proxy make(Args &&... args) {
    static_assert(std::is_base_of_v<Data, Derived>,
                  "Expected template argument to be derived from Proxy::Data");
    return Proxy{std::make_unique<Derived>(std::forward<Args>(args)...)};
  }

  Proxy(const Proxy &u) { data_ = u.data_->clone(); }
  Proxy &operator=(const Proxy &u) {
    if (&u != this) data_ = u.data_->clone();
    return *this;
  }

  Proxy(Proxy &&) = default;
  Proxy &operator=(Proxy &&) = default;

  // Access the contained value. Should NOT return another Proxy.
  Value get() const;

  // Can the contained value be modified
  bool isMutable() const { return data_->isMutable(); }

  // Set the contained value to another value
  void set(Value v);

  // Mutate the contained value by providing a value which can operate on it.
  // The provided reference is not guaranteed to live past the function call.
  void mutate(std::function<void(Value &)> visitor);
};

std::ostream &operator<<(std::ostream &, const Proxy &);
} // namespace extense

#endif /* _LIB_EXTENSE_DETAIL_PROXY_H */
