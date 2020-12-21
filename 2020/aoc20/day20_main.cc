#include <iostream>
#include <iterator>
#include <vector>

#include "aoc20/day20.h"

int main(int argc, char *argv[]) {
  std::vector<aoc20::day20::Tile> tiles(
      std::istream_iterator<aoc20::day20::Tile>{std::cin},
      std::istream_iterator<aoc20::day20::Tile>{});

  std::cout << "Part 1: " << aoc20::day20::Part1(tiles) << std::endl;
  std::cout << "Part 2: " << aoc20::day20::Part2(tiles) << std::endl;
}
