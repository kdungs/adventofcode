#include "aoc20/day13.h"

#include <iostream>
#include <limits>
#include <numeric>
#include <stdexcept>
#include <string>
#include <vector>

#include "absl/strings/numbers.h"
#include "absl/strings/str_split.h"
#include "absl/strings/string_view.h"

namespace aoc20 {
namespace day13 {

std::unordered_map<int, int> ParseBusses(const std::string& line) {
  std::vector<absl::string_view> parts = absl::StrSplit(line, ',');
  std::unordered_map<int, int> busses;
  for (int idx{0}; idx < parts.size(); ++idx) {
    absl::string_view part = parts[idx];
    int bus{0};
    if (absl::SimpleAtoi(part, &bus)) {
      busses[idx] = bus;
    }
  }
  return busses;
}

int Part1(int target, const std::unordered_map<int, int>& busses) {
  auto it = busses.begin();

  int bestbus = it->second;
  int bestdiff = bestbus - target % bestbus;
  ++it;

  for (; it != busses.end(); ++it) {
    int bus = it->second;
    int diff = bus - target % bus;
    if (diff < bestdiff) {
      bestbus = bus;
      bestdiff = diff;
    }
  }
  return bestbus * bestdiff;
}

int64_t Part2(const std::unordered_map<int, int>& busses) {
  int64_t t{0};
  int64_t step{1};
  for (auto [offset, freq] : busses) {
    while ((t + offset) % freq != 0) {
      t += step;
    }
    step *= freq; // All inputs are prime!
  }
  return t;
}

}  // namespace day13
}  // namespace aoc20
