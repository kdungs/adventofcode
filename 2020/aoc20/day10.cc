#include "aoc20/day10.h"

#include <algorithm>
#include <functional>
#include <iostream>
#include <iterator>
#include <numeric>
#include <unordered_set>
#include <vector>

namespace aoc20 {
namespace day10 {

std::vector<int> ArrangeJolts(const std::vector<int>& input) {
  std::vector<int> jolts;
  jolts.reserve(input.size() + 2);
  jolts.push_back(0);
  std::copy(input.begin(), input.end(), std::back_inserter(jolts));
  std::sort(jolts.begin(), jolts.end());
  jolts.push_back(jolts[jolts.size() - 1] + 3);

  return jolts;
}

int Part1(const std::vector<int>& input) {
  auto jolts = ArrangeJolts(input);
  std::vector<int> diffs;
  diffs.reserve(jolts.size() - 1);
  std::adjacent_difference(jolts.begin(), jolts.end(),
                           std::back_inserter(diffs));

  auto cnt1 =
      std::count_if(diffs.begin(), diffs.end(), [](int d) { return d == 1; });
  auto cnt3 =
      std::count_if(diffs.begin(), diffs.end(), [](int d) { return d == 3; });
  return cnt1 * cnt3;
}

int64_t Part2(const std::vector<int>& input) {
  // We can rely on adapters being distinct here!
  auto jolts = ArrangeJolts(input);
  const auto size = jolts.size();

  std::vector<int64_t> variants(size, 0);
  variants[size - 1] = 0;
  variants[size - 2] = 1;

  for (int delta{3}; delta <= size; ++delta) {
    const auto idx = size - delta;
    variants[idx] = variants[idx + 1];
    if (jolts[idx + 2] - jolts[idx] <= 3) {
      // Can skip one
      variants[idx] += variants[idx + 2];
      if (idx + 3 < size) {
        if (jolts[idx + 3] - jolts[idx] <= 3) {
          // Can skip two
          variants[idx] += variants[idx + 3];
        }
      }
    }
  }
  return variants[0];
}

}  // namespace day10
}  // namespace aoc20
