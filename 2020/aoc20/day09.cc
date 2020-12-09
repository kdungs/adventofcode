#include "aoc20/day09.h"

#include <algorithm>
#include <vector>

namespace aoc20 {
namespace day09 {

int Part1(const std::vector<int>& input, std::size_t preamble) {
  auto l = input.begin();
  auto r = l + preamble;

  const auto is_pair = [&l, &r](int num) -> bool {
    for (auto ita{l}; ita != r; ++ita) {
      for (auto itb{ita}; itb != r; ++itb) {
        if (*ita + *itb == num) {
          return true;
        }
      }
    }
    return false;
  };

  while (r != input.end()) {
    int num = *r;
    if (!is_pair(num)) {
      return num;
    }
    ++l;
    ++r;
  }

  return 0;
}

// This is a variation of Kadane's algorithm which solves the maximum subarray
// problem in O(n).
int Part2(const std::vector<int>& input, std::size_t preamble) {
  int target = Part1(input, preamble);

  std::size_t l{0};
  std::size_t r{1};
  int sum{input[l] + input[r]};

  while (l < input.size() - 1 && r < input.size()) {
    if (sum == target) {
      auto [minit, maxit] =
          std::minmax_element(input.begin() + l, input.begin() + r + 1);
      return *minit + *maxit;
    }
    if (sum < target) {
      // Extend range to the right
      ++r;
      sum += input[r];
    } else {
      // Shrink range from the left
      sum -= input[l];
      ++l;
    }
  }

  return 0;
}

}  // namespace day09
}  // namespace aoc20
