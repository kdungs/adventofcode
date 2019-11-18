#ifndef KD_DAY1_H_
#define KD_DAY1_H_

#include <vector>

namespace kd {
namespace day1 {

// Calculates the total frequency from a given vector of increments. This
// solves part 1 of day1.
int CalculateFrequency(const std::vector<int>& increments);

// Finds the first frequency that appears twice when repeatedly applying the
// increments. This solves part 2 of day 1.
int FindFirstDuplicateFrequency(const std::vector<int>& increments);

}  // namespace day1
}  // namespace kd

#endif  // KD_DAY1_H_
