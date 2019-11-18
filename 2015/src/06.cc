#include <iostream>
#include <cassert>
#include <string>

#include <boost/regex.hpp>
#include <range/v3/all.hpp>

#include "helpers.h"

using namespace ranges;

struct Point {
  std::size_t x, y;
};

struct Rectangle {
  Point top_left,
        bottom_right;
};

constexpr bool point_in_rectangle(Point p, Rectangle r) {
  return p.x >= r.top_left.x && p.x <= r.bottom_right.x &&
         p.y >= r.top_left.y && p.y <= r.bottom_right.y;
}

struct Instruction {
  enum class Command {
    ON,
    OFF,
    TOGGLE
  };

  Command c;
  Rectangle area;
};

std::ostream& operator<<(std::ostream& os, Instruction ins) {
  return (os << ins.area.top_left.x);
}

auto command_from_str(const std::string& s) {
  if (s == "turn on") {
    return Instruction::Command::ON;
  }
  if (s == "turn off") {
    return Instruction::Command::OFF;
  }
  return Instruction::Command::TOGGLE;
}

auto parse_instruction(const char* str) {
  const boost::regex exp(
      "^((?:turn (?:(?:on)|(?:off)))|(?:toggle)) (\\d+),(\\d+) through "
      "(\\d+),(\\d+)");
  boost::cmatch mat;
  if (boost::regex_match(str, mat, exp)) {
    return Instruction{
      command_from_str(mat[1]),
      {{std::stoul(mat[2]), std::stoul(mat[3])},
        {std::stoul(mat[4]), std::stoul(mat[5])}}
    };
  }
  assert(false);
}


struct LightView {
  std::vector<Instruction> instructions;

  LightView(std::vector<Instruction> ins) : instructions{std::move(ins)} {
    std::reverse(std::begin(instructions), std::end(instructions));
  }

  bool operator()(const Point p) {
    auto n_toggles = 0u;
    auto result = [&n_toggles](bool value) {
      return (n_toggles % 2 == 0 ? value : !value);
    };

    for (const auto& i : instructions) {
      if (point_in_rectangle(p, i.area)) {
        if (i.c == Instruction::Command::ON) {
          return result(true);
        };
        if (i.c == Instruction::Command::OFF) {
          return result(false);
        };
        ++n_toggles;
      }
    }
    return result(false);
  }
};

struct BrightnessView {
  std::vector<Instruction> instructions;

  auto operator()(const Point p) {
    return accumulate(instructions, 0u, [&p](const auto acc, const auto ins) {
      if (!point_in_rectangle(p, ins.area)) {
        return acc;
      }
      if (ins.c == Instruction::Command::ON) {
        return acc + 1;
      }
      if (ins.c == Instruction::Command::TOGGLE) {
        return acc + 2;
      }
      if (ins.c == Instruction::Command::OFF) {
        if (acc > 0) {
          return acc - 1;
        }
        return 0u;
      }
      assert(false);
    });
  }
};

int main(int argc, char* argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }

  auto data = helpers::read_file<std::string>(argv[1]);
  auto inss = data | view::split('\n')
            | view::transform(to_<std::string>())
            | view::transform([](std::string s) {
                return parse_instruction(s.c_str());
              })
            | to_vector;
  auto lv = LightView{inss};
  auto bv = BrightnessView{inss};

  auto count = 0u;
  auto brightness = 0ul;
  for (auto x = 0u; x < 1000; ++x) {
    for (auto y = 0u; y < 1000; ++y) {
      if (lv({x, y})) {
        ++count;
      };
      brightness += bv({x, y});
    }
  }

  std::cout << "Count: " << count << '\n';
  std::cout << "Brightness: " << brightness << '\n';
}
