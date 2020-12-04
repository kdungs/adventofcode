#include <iostream>

#include "aoc20/day04.h"

int main(int argc, char *argv[]) {
  std::istreambuf_iterator<char> begin(std::cin), end;
  std::string input(begin, end);

  auto passports = aoc20::day04::ParsePassports(input);
  std::cout << "Part 1: " << aoc20::day04::Part1(passports) << std::endl;
  std::cout << "Part 2: " << aoc20::day04::Part2(passports) << std::endl;
}
