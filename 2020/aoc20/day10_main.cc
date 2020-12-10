#include <iostream>
#include <vector>

#include "aoc20/day10.h"

int main(int argc, char *argv[]) {
  std::vector<int> input;
  int in;
  while (std::cin >> in) {
    input.push_back(in);
  }

  std::cout << "Part 1: " << aoc20::day10::Part1(input) << std::endl;
  std::cout << "Part 2: " << aoc20::day10::Part2(input) << std::endl;
}
