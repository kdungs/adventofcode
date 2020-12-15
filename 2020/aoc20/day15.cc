#include "aoc20/day15.h"

#include <array>
#include <iostream>
#include <unordered_map>

namespace aoc20 {
namespace day15 {

struct AgeInfo {
  AgeInfo() : last{0}, prev{0} {}

  // Last time the number was said.
  int last;
  // Previous time before last the number was said.
  int prev;

  void update(int turn) {
    prev = last;
    last = turn;
  }

  int num() const {
    if (prev == 0) {
      // Last time was the first time.
      return 0;
    }
    return last - prev;
  }
};

int Part1(const std::vector<int>& input) {
  int turn{1};
  std::array<AgeInfo, 2020> infos = {};
  for (int in : input) {
    infos[in].update(turn);
    ++turn;
  }

  int last = input.back();
  while (turn <= 2020) {
    last = infos[last].num();
    infos[last].update(turn);
    ++turn;
  }
  return last;
}

int Part2(const std::vector<int>& input) {
  int turn{1};
  std::unordered_map<int, AgeInfo> infos;
  for (int in : input) {
    infos[in].update(turn);
    ++turn;
  }

  int last = input.back();
  while (turn <= 30000000) {
    last = infos[last].num();
    infos[last].update(turn);
    ++turn;
  }
  return last;
}

}  // namespace day15
}  // namespace aoc20
