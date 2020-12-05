#ifndef AOC18_DAY01_H_
#define AOC18_DAY01_H_

#include <vector>

namespace aoc18 {
namespace day01 {

// Calculates the total frequency from a given vector of increments. This
// solves part 1 of day 1.
int CalculateFrequency(const std::vector<int>& increments);

// Finds the first frequency that appears twice when repeatedly applying the
// increments. This solves part 2 of day 1.
int FindFirstDuplicateFrequency(const std::vector<int>& increments);

}  // namespace day01
}  // namespace aoc18

#endif  // AOC18_DAY01_H_
