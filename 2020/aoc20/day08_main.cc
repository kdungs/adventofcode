#include <iostream>

#include "aoc20/day08.h"

int main(int argc, char *argv[]) {
  auto maybe_prog = aoc20::day08::ParseProg(std::cin);
  if (!maybe_prog.has_value()) {
    std::cerr << "unable to parse program\n";
    return 1;
  }
  auto prog = maybe_prog.value();

  std::cout << "Part 1: " << aoc20::day08::Part1(prog) << std::endl;
  std::cout << "Part 2: " << aoc20::day08::Part2(prog) << std::endl;
}
