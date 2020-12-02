#include <iostream>
#include <string>
#include <vector>

#include "aoc20/day02.h"

int main(int argc, char *argv[]) {
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(std::cin, line)) {
    lines.push_back(line);
  }

  auto maybe_inputs = aoc20::day02::ParseInputs(lines);
  if (!maybe_inputs.has_value()) {
    std::cerr << "unable to parse input\n";
    return 1;
  }
  auto inputs = maybe_inputs.value();

  std::cout << "Part 1: " << aoc20::day02::Part1(inputs) << std::endl;
  std::cout << "Part 2: " << aoc20::day02::Part2(inputs) << std::endl;
}
