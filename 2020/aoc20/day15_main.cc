#include <iostream>
#include <vector>

#include "aoc20/day15.h"

int main(int argc, char *argv[]) {
  std::vector<int> input;
  int num;
  char sep;
  while (std::cin >> num >> sep) {
    input.push_back(num);
  }

  std::cout << "Part 1: " << aoc20::day15::Part1(input) << std::endl;
  std::cout << "Part 2: " << aoc20::day15::Part2(input) << std::endl;
}
