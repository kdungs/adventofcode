#include <iostream>

#include "aoc20/day07.h"

int main(int argc, char *argv[]) {
  auto maybe_rules = aoc20::day07::ParseRules(std::cin);
  if (!maybe_rules.has_value()) {
    std::cerr << "unable to parse rules";
    return 1;
  }
  auto rules = maybe_rules.value();
  std::cout << "Part 1: " << aoc20::day07::Part1(rules) << std::endl;
  std::cout << "Part 2: " << aoc20::day07::Part2(rules) << std::endl;
}
