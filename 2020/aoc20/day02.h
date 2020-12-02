#ifndef AOC20_DAY02_H_
#define AOC20_DAY02_H_

#include <optional>
#include <string>
#include <vector>

namespace aoc20 {
namespace day02 {

struct Input {
  int left;
  int right;
  char letter;
  std::string password;
};

std::optional<std::vector<Input>> ParseInputs(
    const std::vector<std::string>& lines);

int Part1(const std::vector<Input>& inputs);

int Part2(const std::vector<Input>& inputs);

}  // namespace day02
}  // namespace aoc20

#endif  // AOC20_DAY02_H_
