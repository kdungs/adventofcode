#include <iostream>

#include "aoc20/day06.h"

int main(int argc, char *argv[]) {
  const auto blocks = aoc20::day06::ParseInput(std::cin);
  std::cout << "Part 1: " << aoc20::day06::Part1(blocks) << std::endl;
  std::cout << "Part 2: " << aoc20::day06::Part2(blocks) << std::endl;
}
