#include <iostream>
#include <string>
#include <vector>

#include "aoc18/day16.h"

int main(int argc, char* argv[]) {
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(std::cin, line)) {
    lines.push_back(line);
  }

  const auto maybe_samples_and_program = aoc18::day16::ParseInput(lines);
  if (!maybe_samples_and_program.has_value()) {
    std::cerr << "unable to parse input" << std::endl;
    return 1;
  }
  const auto [samples, program] = maybe_samples_and_program.value();
  std::cout << "Part 1: " << aoc18::day16::Part1(samples) << std::endl;
  std::cout << "Part 2: " << aoc18::day16::Part2(samples, program) << std::endl;
}
