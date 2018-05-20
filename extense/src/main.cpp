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
#include <extense/parser.hpp>
#include <extense/representation.hpp>

// static void dumpTokens(const std::vector<extense::Token> &tokens) {
//   for (const auto &token : tokens) {
//     if (token.text() == "\n")
//       std::cout << '\n';
//     else
//       std::cout << '|' << token.text() << '/' << token.type();

//     // If there is any data in the token, print it
//     std::visit(
//         [](const auto &data) {
//           if constexpr (!std::is_same_v<std::decay_t<decltype(data)>,
//                                         std::monostate>)
//             std::cout << '/' << data;
//         },
//         token.data());
//   }

//   std::cout << '\n';
// }

using namespace extense;
using namespace extense::literals;

template <typename Func>
static void injectFunction(Scope &scope, std::string name, Func f) {
  scope.createIdentifier(std::move(name)) =
      Value{Scope{[=](auto &, auto &v) { return f(v); }}};
}

static String lippincott() {
  try {
    throw;
  } catch (const ExceptionWrapper &e) {
    return String{e.location()} + ": "_es + String{e.data().error()};
  } catch (const Exception &e) { return String{e.error()}; } catch (...) {
    return "Unknown exception"_es;
  }
}

static void injectAll(Scope &global) {
  injectFunction(global, "getInt", [](auto &) {
    int i;
    std::cin >> i;
    return Value{Int{i}};
  });
  injectFunction(global, "print", [](auto &v) {
    std::cout << v;
    return noneValue;
  });
  injectFunction(global, "println", [](auto &v) {
    std::cout << v << '\n';
    return noneValue;
  });
  injectFunction(global, "for", [](auto &v) {
    auto &args = get<List>(v);
    auto lower = get<Int>(args[0_ei]);
    auto upper = get<Int>(args[1_ei]);
    auto &func = mutableGet<Scope>(args[2_ei]);

    for (auto i = lower; i < upper; i.value++) func(i);
    return noneValue;
  });
  injectFunction(global, "?", [](auto &v) {
    auto &args = get<List>(v);
    auto cond = get<Bool>(args[0_ei]);
    auto &func = mutableGet<Scope>(args[1_ei]);
    if (cond) { func(); }
    return Value{cond};
  });

  auto switchValue = [](const Value &v) -> Value {
    auto &args = get<List>(v);
    if (args.value.size() % 2 != 0)
      throw ArgumentError{"Expected an even number of arguments"};
    for (auto i = 0_ei; i < args.size(); i += 2_ei) {
      auto condition = get<Bool>(args[i]);
      if (!condition) continue;

      auto value = args[i + 1_ei];
      return value;
    }

    return noneValue;
  };
  injectFunction(global, "switchValue", switchValue);
  injectFunction(global, "switch", [&](const Value &v) -> Value {
    auto toCall = switchValue(v);
    if (toCall.is<None>()) return Value{Bool::f};
    if (!toCall.is<Scope>()) throw ArgumentError{"Expected scope to call"};
    get<Scope>(toCall)();
    return Value{Bool::t};
  });

  injectFunction(global, "forEach", [](const Value &v) {
    auto &args = get<List>(v);
    auto &list = get<List>(args[0_ei]);
    auto &func = mutableGet<Scope>(args[1_ei]);
    for (auto i = 0_ei; i < list.size(); i.value++) func(list[i], i);
    return noneValue;
  });
  injectFunction(global, "anyOf", [](const Value &v) {
    // ...
    return noneValue;
  });
  injectFunction(global, "size", [](const Value &v) -> Value {
    if (v.is<Map>()) return Value{get<Map>(v).size()};
    if (v.is<List>()) return Value{get<List>(v).size()};
    if (v.is<String>()) return Value{get<String>(v).size()};
    throw ArgumentError{"Can only call 'size' with String, Map or List type"};
  });

  auto tryExec = [](const Value &v) -> Value {
    auto &args = get<List>(v);
    auto &toTry = mutableGet<Scope>(args[0_ei]);
    auto &catchFunc = mutableGet<Scope>(args[1_ei]);
    try {
      return toTry();
    } catch (...) { return catchFunc(lippincott()); }
  };

  injectFunction(global, "try", tryExec);

  injectFunction(global, "set", [](const Value &v) -> Value {
    auto &args = mutableGet<List>(v);
    auto &toSet = args[0_ei];
    auto &assignValue = args[1_ei];

    if (!toSet.is<Reference>()) toSet = assignValue;
    *get<Reference>(toSet) = assignValue.flatten();
    return noneValue;
  });

  injectFunction(global, "copy", [](const Value &v) -> Value {
    if (!v.is<Reference>()) return v;
    auto copied = *get<Reference>(v);
    return Value{copied};
  });
}

int main(int argc, const char *argv[]) {
  // std::cout << "Extense version " << version << "\n\n";

  if (argc != 2) {
    std::cerr << "Expected input file\n";
    return 1;
  }

  std::ifstream file{argv[1]};
  if (!file.good()) {
    std::cerr << "Could not find input file\n";
    return 1;
  }
  std::stringstream buffer;
  buffer << file.rdbuf();
  auto source = buffer.str();

  std::unique_ptr<Expr> expr;
  try {
    expr = parse(source);
  } catch (const LexingError &error) {
    std::cerr << "Error tokenizing file at " << error.throwLocation() << ": \""
              << error.what() << "\"\n";
    return 1;
  } catch (const ParseError &e) {
    std::cerr << "Encountered ParseError at " << e.throwLocation()
              << ", with token '" << e.tokenText() << "' (type '"
              << e.tokenType() << "'), and with message '" << e.what() << "'\n";
    return 1;
  }

  Scope global{[](auto &, auto &) { return noneValue; }};
  injectAll(global);

  class TypeBuilder : public UserObject::Data {
    std::vector<std::string> result;

  public:
    std::unique_ptr<UserObject::Data> clone() const override {
      auto m = std::make_unique<TypeBuilder>();
      std::cout << "COPY\n";
      m->result = result;
      return m;
    }

    void print(std::ostream &os) const override {
      std::cout << "Result: " << index(Value{"result"_es}).get();
    }

    Value addEquals(const Value &v) override {
      result.push_back(std::string(v.typeAsString()));
      return noneValue;
    }

    Proxy index(const Value &index) const override {
      if (index == Value{"result"_es})
        return Proxy::make<FieldProxy<std::vector<std::string>, true, false>>(
            result);

      throw InvalidBinaryOperation{"UserObject", index.typeAsString(),
                                   "Key not present"};
    }

    Bool equal(const Value &v) const override {
      if (!v.is<UserObject>()) return Bool::f;
      const auto &uo = get<UserObject>(v);
      const auto *tb = dynamic_cast<const TypeBuilder *>(&uo.data());
      if (!tb) return Bool::f;
      return Bool{result == tb->result};
    }

    Bool notEqual(const Value &v) const override { return !equal(v); }
  };

  injectFunction(global, "CppTypeBuilder", [](const Value &v) -> Value {
    return Value{UserObject::make<TypeBuilder>()};
  });

  auto evaluated = constEval(global, *expr);
  auto &s = get<Scope>(evaluated);
  try {
    s();
  } catch (...) { std::cout << lippincott() << '\n'; }

  return 0;
}
