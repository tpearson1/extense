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

  // Access the contained value
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
