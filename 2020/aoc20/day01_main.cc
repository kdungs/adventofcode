#include <iostream>
#include <vector>

#include "aoc20/day01.h"

int main(int argc, char *argv[]) {
  std::vector<int> inputs;
  int input;
  while (std::cin >> input) {
    inputs.push_back(input);
  }
  std::cout << "Part 1: " << aoc20::day01::Part1(inputs) << std::endl;
  std::cout << "Part 2: " << aoc20::day01::Part2(inputs) << std::endl;
}
