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

using Program = std::vector<Instruction>;

struct Sample {
  Registers before;
  Registers after;
  Instruction instruction;
};

using Samples = std::vector<Sample>;

using SamplesAndProgram = std::tuple<Samples, Program>;

std::optional<SamplesAndProgram> ParseInput(
    const std::vector<std::string>& lines);

int Part1(const Samples& samples);

int Part2(const Samples& samples, const Program& program);

}  // namespace day16
}  // namespace aoc18

#endif  // AOC18_DAY16_H_
