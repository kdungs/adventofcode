#include <iostream>
#include <iterator>
#include <vector>

#include "aoc20/day20.h"

int main(int argc, char *argv[]) {
  std::vector<aoc20::day20::Tile> tiles(
      std::istream_iterator<aoc20::day20::Tile>{std::cin},
      std::istream_iterator<aoc20::day20::Tile>{});

  std::cout << tiles[0] << std::endl;
  auto wob = tiles[0].WithoutBorder();
  for (int y{0}; y < 8; ++y) {
    for (int x{0}; x < 8; ++x) {
      std::cout << wob[y * 8 + x];
    }
    std::cout << std::endl;
  }

  std::cout << "Part 1: " << aoc20::day20::Part1(tiles) << std::endl;
  std::cout << "Part 2: " << aoc20::day20::Part2(tiles) << std::endl;
}
