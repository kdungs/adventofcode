#include <iostream>
#include <string>

#include "aoc18/day19.h"

int main(int argc, char* argv[]) {
  std::string input(std::istreambuf_iterator<char>{std::cin},
                    std::istreambuf_iterator<char>{});

  auto maybe_prog = aoc18::day19::ParseProg(input);
  if (!maybe_prog.has_value()) {
    std::cerr << "unable to parse program\n";
    return 1;
  }
  auto prog = maybe_prog.value();
  std::cout << "Part 1: " << aoc18::day19::Part1(prog) << std::endl;
  std::cout << "Part 2: " << aoc18::day19::Part2(prog) << std::endl;
}
