#include <iostream>
#include <string>
#include <vector>

#include "aoc20/day19.h"

int main(int argc, char *argv[]) {
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(std::cin, line)) {
    lines.push_back(line);
  }

  std::cout << "Part 1: " << aoc20::day19::Part1(lines) << std::endl;
  std::cout << "Part 2: " << aoc20::day19::Part2(lines) << std::endl;
}
