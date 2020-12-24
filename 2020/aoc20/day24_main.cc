#include <iostream>
#include <vector>

#include "aoc20/day24.h"

int main(int argc, char *argv[]) {
  std::vector<aoc20::day24::HexPath> paths;
  aoc20::day24::HexPath path;
  while (std::cin >> path) {
    paths.push_back(path);
    path.clear();
  }

  std::cout << "Part 1: " << aoc20::day24::Part1(paths) << std::endl;
  std::cout << "Part 2: " << aoc20::day24::Part2(paths) << std::endl;
}
