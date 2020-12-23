#include <iostream>
#include <vector>

#include "aoc20/day23.h"

int main(int argc, char *argv[]) {
  std::string line;
  std::cin >> line;
  std::vector<int> input;
  for (char c : line) {
    input.push_back(c - '0');
  }

  std::cout << "Part 1: " << aoc20::day23::Part1(input) << std::endl;
  std::cout << "Part 2: " << aoc20::day23::Part2(input) << std::endl;
}
