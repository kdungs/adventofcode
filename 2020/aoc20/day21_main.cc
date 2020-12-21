#include <iostream>
#include <iterator>
#include <vector>

#include "aoc20/day21.h"

int main(int argc, char *argv[]) {
  std::vector<aoc20::day21::Entry> entries(
      std::istream_iterator<aoc20::day21::Entry>{std::cin},
      std::istream_iterator<aoc20::day21::Entry>{});

  std::cout << "Part 1: " << aoc20::day21::Part1(entries) << std::endl;
  std::cout << "Part 2: " << aoc20::day21::Part2(entries) << std::endl;
}
