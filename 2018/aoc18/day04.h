#ifndef AOC18_DAY04_H_
#define AOC18_DAY04_H_

#include <bitset>
#include <optional>
#include <vector>

namespace aoc18 {
namespace day04 {

using Minutes = std::bitset<60>;

struct Shift {
  int guard;
  Minutes asleep_on_minute;
};

using ShiftPlan = std::vector<Shift>;

std::optional<ShiftPlan> ParseShiftPlan(std::vector<std::string> lines);

int SolvePart1(const ShiftPlan& sp);

int SolvePart2(const ShiftPlan& sp);

}  // namespace day04
}  // namespace aoc18

#endif  // AOC18_DAY04_H_
