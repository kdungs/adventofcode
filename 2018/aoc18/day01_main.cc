#include <iostream>
#include <vector>

#include "aoc18/day01.h"

int main(int argc, char* argv[]) {
  // Read problem input from stdin
  std::vector<int> increments = {};
  int increment;
  while (std::cin >> increment) {
    increments.push_back(increment);
  }

  std::cout << "Part 1: " << aoc18::day01::CalculateFrequency(increments)
            << '\n';
  std::cout << "Part 2: "
            << aoc18::day01::FindFirstDuplicateFrequency(increments) << '\n';
}
