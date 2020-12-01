#include "aoc20/day01.h"

#include <unordered_set>
#include <vector>

namespace aoc20 {
namespace day01 {

int Part1(const std::vector<int>& input) {
  std::unordered_set<int> numbers(input.begin(), input.end());
  for (const int number : input) {
    if (numbers.count(2020 - number) > 0) {
      return number * (2020 - number);
    }
  }
  return -1;
}

int Part2(const std::vector<int>& input) {
  std::unordered_set<int> numbers(input.begin(), input.end());
  for (const int n1 : input) {
    for (const int n2 : input) {
      auto n3 = 2020 - n1 - n2;
      if (numbers.count(n3) > 0) {
        return n1 * n2 * n3;
      }
    }
  }
  return -1;

  return 0;
}

}  // namespace day01
}  // namespace aoc20
