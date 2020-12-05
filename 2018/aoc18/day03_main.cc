#include <iostream>
#include <string>
#include <vector>

#include "aoc18/day03.h"

int main(int argc, char *argv[]) {
  // Read problem input from stdin.
  std::vector<aoc18::day03::Rectangle> claims = {};
  std::string line;
  while (std::getline(std::cin, line)) {
    absl::optional<aoc18::day03::Rectangle> maybe_rect =
        aoc18::day03::ParseRectangle(line);
    if (maybe_rect) {
      claims.push_back(*maybe_rect);
    }
  }

  const aoc18::day03::Grid grid = aoc18::day03::Grid::Build(claims);

  std::cout << "Part 1: " << grid.OverlapArea() << '\n';
  std::cout << "Part 2: "
            << grid.FindNonOverlapping(claims)
                   .value_or(aoc18::day03::Rectangle(0, 0, 0, 0, 0))
                   .id()
            << '\n';
}
