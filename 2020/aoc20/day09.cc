#include "aoc20/day09.h"

#include <algorithm>
#include <deque>
#include <vector>

namespace aoc20 {
namespace day09 {

int Part1(const std::vector<int>& input, std::size_t preamble) {
  // Read first preamble numbers into ringbuffer of size preamble.
  // For each next number check if it's a pair; that's O(preamble^2) but O(1)
  // since preamble is a constant.
  std::deque<int> ring(input.begin(), input.begin() + preamble);

  const auto is_pair = [&ring](int num) -> bool {
    for (int a : ring) {
      for (int b : ring) {
        if (a + b == num) {
          return true;
        }
      }
    }
    return false;
  };

  for (std::size_t idx{preamble}; idx < input.size(); ++idx) {
    int num = input[idx];
    if (!is_pair(num)) {
      return num;
    }
    ring.pop_front();
    ring.push_back(num);
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
