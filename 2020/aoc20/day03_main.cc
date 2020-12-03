#include <iostream>
#include <string>
#include <vector>

#include "aoc20/day03.h"

int main(int argc, char *argv[]) {
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(std::cin, line)) {
    lines.push_back(line);
  }

  auto maybe_forest = aoc20::day03::ParseForest(lines);
  if (!maybe_forest.has_value()) {
    std::cerr << "unable to parse forest\n";
    return EXIT_FAILURE;
  }
  auto forest = maybe_forest.value();

  std::cout << "Part 1: " << aoc20::day03::Part1(forest) << std::endl;
  std::cout << "Part 2: " << aoc20::day03::Part2(forest) << std::endl;
}
