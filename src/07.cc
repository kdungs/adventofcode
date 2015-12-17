#include <cassert>
#include <functional>
#include <iostream>
#include <regex>
#include <string>
#include <unordered_map>

#include <range/v3/all.hpp>

#include "helpers.h"

class Expression {
 public:
  using Identifier = std::string;
  using Mapping = std::unordered_map<Identifier, Expression>;

  template <typename E>
  Expression(E e) : self_{std::make_unique<_Model<E>>(std::move(e))} {}

  auto operator()(Mapping& map) const noexcept { return (*self_)(map); }

 private:
  struct _Concept {
    virtual ~_Concept() = default;
    virtual uint16_t operator()(Mapping&) const noexcept = 0;
  };
  template <typename T>
  struct _Model final : _Concept {
    T data;
    _Model(T d) : data{std::move(d)} {}

    uint16_t operator()(Mapping& map) const noexcept override {
      return data(map);
    }
  };
  std::shared_ptr<const _Concept> self_;
};

struct Value {
  const uint16_t value;
  constexpr uint16_t operator()(Expression::Mapping&) const noexcept {
    return value;
  }
};

struct Variable {
  const Expression::Identifier identifier;
  /*constexpr*/ uint16_t operator()(Expression::Mapping& map) const noexcept {
    auto result = map.at(identifier)(map);
    map.at(identifier) = Value{result};
    return result;
  }
};

template <typename OP>
struct Unary {
  const Expression exp;
  constexpr uint16_t operator()(Expression::Mapping& map) const noexcept {
    return OP{}(exp(map));
  }
};

template <typename OP>
struct Binary {
  const Expression lhs;
  const Expression rhs;
  constexpr uint16_t operator()(Expression::Mapping& map) const noexcept {
    return OP{}(lhs(map), rhs(map));
  }
};

struct BitwiseComplementImpl {
  template <typename T>
  constexpr T operator()(const T& x) {
    return ~x;
  };
};

struct LeftShiftImpl {
  template <typename T>
  constexpr T operator()(const T& lhs, const T& rhs) {
    return lhs << rhs;
  };
};

struct RightShiftImpl {
  template <typename T>
  constexpr T operator()(const T& lhs, const T& rhs) {
    return lhs >> rhs;
  };
};

using Not = Unary<BitwiseComplementImpl>;
using And = Binary<std::bit_and<uint16_t>>;
using Or = Binary<std::bit_or<uint16_t>>;
using LeftShift = Binary<LeftShiftImpl>;
using RightShift = Binary<RightShiftImpl>;

auto splitLine(const std::string& line, const std::string token) {
  const auto s = token.size();
  const auto i = line.rfind(token);
  return std::make_tuple(std::string{std::begin(line), std::begin(line) + i},
                         std::string{std::begin(line) + i + s, std::end(line)});
}

struct Parser {
  Expression parseExpression(const std::string& exp) {
    /** There are four different types of expressions:
     * value
     * variable
     * unary (expression)
     * (expression) binary (expression)
     *
     * with the limitation that (expression) here can really only be value |
     * variable
     *
     * ^(\d+)$                 -> Value{\1}
     * ^([a-z]+)$              -> Variable{\1}
     * ^([A-Z]+) (.+)$         -> Unary<\1>{parse(\2)}
     * ^([^ ]+) ([A-Z]+) (.+)$ -> Binary<\2>{parse(\1), parse(\3)}
     *
     * Also I guess for the recursive calls string_view would be super helpful…
     */
    const auto rvalue = std::regex{"^(\\d+)$"};
    const auto rvariable = std::regex{"^([a-z]+)$"};
    const auto runary = std::regex{"^([A-Z]+) (.+)$"};
    const auto rbinary = std::regex{"^([^ ]+) ([A-Z]+) (.+)$"};

    std::smatch mat;
    if (std::regex_match(exp, mat, rvalue)) {
      return Value{static_cast<uint16_t>(std::stoul(mat[1].str()))};
    }
    if (std::regex_match(exp, mat, rvariable)) {
      return Variable{mat[1].str()};
    }
    if (std::regex_match(exp, mat, runary)) {
      const auto command = mat[1].str();
      const auto subexpr = parseExpression(mat[2].str());
      if (command == "NOT") {
        return Not{subexpr};
      }
      assert(false);
    }
    if (std::regex_match(exp, mat, rbinary)) {
      const auto command = mat[2].str();
      const auto subexpl = parseExpression(mat[1].str());
      const auto subexpr = parseExpression(mat[3].str());
      if (command == "AND") {
        return And{subexpl, subexpr};
      }
      if (command == "OR") {
        return Or{subexpl, subexpr};
      }
      if (command == "LSHIFT") {
        return LeftShift{subexpl, subexpr};
      }
      if (command == "RSHIFT") {
        return RightShift{subexpl, subexpr};
      }
      assert(false);
    }
    assert(false);
  }

  void parseLine(const std::string& line) {
    std::string exp, var;
    std::tie(exp, var) = splitLine(line, " -> ");
    map_.emplace(var, parseExpression(exp));
  }

  uint16_t evaluate(Expression::Identifier i) { return map_.at(i)(map_); }

  Expression::Mapping map_;
};

using namespace ranges;

int main(int argc, char* argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }

  auto ifs = std::ifstream{argv[1]};
  auto lines = getlines(ifs) | to_vector;
  auto p1 = Parser{};
  for (const auto& line : lines) {
    p1.parseLine(line);
  }
  auto p2 = p1;
  auto a = p1.evaluate("a");
  std::cout << "a = " << a << '\n';

  p2.map_.at("b") = Value{a};
  std::cout << "a = " << p2.evaluate("a") << '\n';
}
