#include <iostream>
#include <string>
#include <vector>

#include "kd/day3.h"

int main(int argc, char *argv[]) {
  // Read problem input from stdin.
  std::vector<kd::day3::Rectangle> claims = {};
  std::string line;
  while (std::getline(std::cin, line)) {
    absl::optional<kd::day3::Rectangle> maybe_rect =
        kd::day3::ParseRectangle(line);
    if (maybe_rect) {
      claims.push_back(*maybe_rect);
    }
  }

  const kd::day3::Grid grid = kd::day3::Grid::Build(claims);

  std::cout << "Part 1: " << grid.OverlapArea() << '\n';
  std::cout << "Part 2: "
            << grid.FindNonOverlapping(claims)
                   .value_or(kd::day3::Rectangle(0, 0, 0, 0, 0))
                   .id()
            << '\n';
}
