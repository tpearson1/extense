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
  const std::string type_;

public:
  explicit Exception(std::string type = "Exception") : type_(std::move(type)) {}

  const std::string &type() const { return type_; }
  const char *what() const noexcept override { return "Unknown Exception"; }
};

class LocatableError : public Exception {
  Source::Location throwLocation_;
  std::string error_;
  static inline std::string type = "LocatableError";

public:
  explicit LocatableError(Source::Location loc, std::string error)
      : Exception(type), throwLocation_(std::move(loc)),
        error_(std::move(error)) {}

  const Source::Location &throwLocation() const { return throwLocation_; }

  const char *what() const noexcept override { return error_.c_str(); }
};
} // namespace extense

#endif // _LIB_EXTENSE__EXCEPTION_HPP
