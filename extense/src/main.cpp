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

#include <config.h>
#include <extense/token.h>

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
    auto disp = token.text();
    if (disp == "\n") disp = "\\n";
    std::cout << disp << '|' << token.type() << '\n';
  }

  return 0;
}
