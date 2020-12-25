#include "aoc20/day25.h"

namespace aoc20 {
namespace day25 {

constexpr int kSubject{7};
constexpr int kMod{20201227};

int DetermineLoop(int pub) {
  int value{1};
  int loop{0};
  while (value != pub) {
    value = (value * kSubject) % kMod;
    ++loop;
  }
  return loop;
}

int Transform(int loop, int64_t subject) {
  int value{1};
  for (int i{0}; i < loop; ++i) {
    value = (value * subject) % kMod;
  }
  return value;
}

int Part1(int pub_card, int pub_door) {
  // Figure out loop size by brute force.
  int loop_card = DetermineLoop(pub_card);
  return Transform(loop_card, pub_door);
}

}  // namespace day25
}  // namespace aoc20
