#include "aoc20/day03.h"

#include <initializer_list>
#include <optional>
#include <string>
#include <tuple>
#include <vector>

namespace aoc20 {
namespace day03 {

std::optional<Forest> ParseForest(const std::vector<std::string>& lines) {
  int height = lines.size();
  if (height < 1) {
    return std::nullopt;
  }
  int width = lines[0].size();
  if (width < 1) {
    return std::nullopt;
  }
  Forest f{width, height};
  for (int y = 0; y < height; ++y) {
    for (int x = 0; x < width; ++x) {
      if (lines[y][x] == '#') {
        f.AddTree(x, y);
      }
    }
  }
  return f;
}

int CountTreesOnSlope(const Forest& f, int dx, int dy) {
  int x = 0;
  int y = 0;
  int ntrees = 0;

  while (y < f.height()) {
    x += dx;
    y += dy;  // Assuming we only get "good" values for dy here...
    ntrees += f.IsTree(x, y);
  }

  return ntrees;
}

int Part1(const Forest& forest) { return CountTreesOnSlope(forest, 3, 1); }

int64_t Part2(const Forest& forest) {
  int64_t product = 1;

  for (auto [dx, dy] : std::initializer_list<std::tuple<int, int>>{
           {1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}}) {
    product *= CountTreesOnSlope(forest, dx, dy);
  }

  return product;
}

}  // namespace day03
}  // namespace aoc20
