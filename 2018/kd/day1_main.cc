#include <iostream>
#include <vector>

#include "kd/day1.h"

int main(int argc, char* argv[]) {
  // Read problem input from stdin
  std::vector<int> increments = {};
  int increment;
  while (std::cin >> increment) {
    increments.push_back(increment);
  }

  std::cout << "Part 1: " << kd::day1::CalculateFrequency(increments) << '\n';
  std::cout << "Part 2: " << kd::day1::FindFirstDuplicateFrequency(increments)
            << '\n';
}
