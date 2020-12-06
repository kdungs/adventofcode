#ifndef AOC18_DAY16_H_
#define AOC18_DAY16_H_

#include <array>
#include <functional>
#include <memory>
#include <optional>
#include <regex>
#include <string>
#include <vector>

namespace aoc18 {
namespace day16 {

using Registers = std::array<int, 4>;

struct Instruction {
  int code;
  int a;
  int b;
  int c;
};

struct Sample {
  Registers before;
  Registers after;
  Instruction instruction;
};

std::optional<std::vector<Sample>> ParseSamples(const std::vector<std::string>& lines);

int Part1(const std::vector<Sample>& samples);

}  // namespace day16
}  // namespace aoc18

#endif  // AOC18_DAY16_H_
