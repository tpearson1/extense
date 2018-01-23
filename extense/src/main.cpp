/*
-------------------------------------------------------------------------------
This file is part of Extense
-------------------------------------------------------------------------------
Copyright (c) 2017 Thomas Pearson

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

#include <fstream>
#include <iostream>
#include <sstream>
#include <type_traits>

#include <config.hpp>
#include <extense/token.hpp>

int main(int /*argc*/, const char * /*argv*/ []) {
  std::cout << "Extense version " << extense::version << '\n';

  std::ifstream file{"language.xts"};
  std::stringstream buffer;
  buffer << file.rdbuf();
  extense::Source s{buffer.str()};

  std::vector<extense::Token> tokens;
  try {
    tokens = extense::tokenize(s);
  } catch (const extense::LexingError &error) {
    std::cerr << "Error tokenizing file at " << error.location() << ": \""
              << error.what() << "\"\n";
    return 1;
  }

  for (const auto &token : tokens) {
    if (token.text() == "\n")
      std::cout << '\n';
    else
      std::cout << '|' << token.text() << '/' << token.type();

    // If there is any data in the token, print it
    std::visit(
        [](const auto &data) {
          if constexpr (!std::is_same_v<std::decay_t<decltype(data)>,
                                        std::monostate>)
            std::cout << '/' << data;
        },
        token.data());
  }

  std::cout << '\n';

  return 0;
}
