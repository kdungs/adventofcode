#ifndef KD_DAY4_H_
#define KD_DAY4_H_

#include <bitset>
#include <optional>
#include <vector>

namespace kd {
namespace day4 {

using Minutes = std::bitset<60>;

struct Shift {
  int guard;
  Minutes asleep_on_minute;
};

using ShiftPlan = std::vector<Shift>;

std::optional<ShiftPlan> ParseShiftPlan(std::vector<std::string> lines);

int SolvePart1(const ShiftPlan& sp);

int SolvePart2(const ShiftPlan& sp);

}  // namespace day4
}  // namespace kd

#endif  // KD_DAY4_H_
