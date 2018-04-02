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

#ifndef _LIB_EXTENSE__EXCEPTION_HPP
#define _LIB_EXTENSE__EXCEPTION_HPP

#include <exception>
#include <string>

#include <extense/source.hpp>

namespace extense {
class Exception : public std::exception {
  std::string type_ = "Exception";
  std::string error_;

protected:
  void setType(std::string type) { type_ = std::move(type); }
  void setError(std::string error) { error_ = std::move(error); }

public:
  explicit Exception(std::string error) : error_(std::move(error)) {}

  const std::string &type() const { return type_; }

  const char *what() const noexcept override { return error_.c_str(); }
  const std::string &error() const noexcept { return error_; }
};

class LocatableError : public Exception {
  Source::Location throwLocation_;

public:
  LocatableError(std::string error, Source::Location loc)
      : Exception(std::move(error)), throwLocation_(std::move(loc)) {
    setType("LocatableError");
  }

  const Source::Location &throwLocation() const { return throwLocation_; }
};

/*
 * Exception thrown in cases which are thought to be impossible.
 */
class LogicError : public Exception {
public:
  explicit LogicError(std::string error) : Exception(std::move(error)) {
    setType("LogicError");
  }
};

/*
 * Exception thrown in cases which are thought to be impossible by the API. If
 * this is thrown there is a bug in our API.
 */
class InternalLogicError : public Exception {
public:
  explicit InternalLogicError(std::string error) : Exception(std::move(error)) {
    setType("InternalLogicError");
  }
};

/*
 * Exception thrown when asked to get a mutable value from a const type.
 */
class MutableAccessError : public Exception {
public:
  explicit MutableAccessError(
      std::string error = "Unable to get a mutable value type")
      : Exception(std::move(error)) {
    setType("MutableAccessError");
  }
};

/*
 * Exception thrown when unable to constrain a FlatValue.
 */
class ConstraintFailure : public Exception {
public:
  explicit ConstraintFailure(std::string error = "Could not constrain type")
      : Exception(std::move(error)) {
    setType("ConstraintFailure");
  }
};

/*
 * Exception thrown when the number of arguments given to a function is invalid,
 * or argument value(s) are invalid.
 */
class ArgumentError : public Exception {
public:
  explicit ArgumentError(std::string error) : Exception(std::move(error)) {
    setType("ArgumentError");
  }
};
} // namespace extense

#endif // _LIB_EXTENSE__EXCEPTION_HPP
