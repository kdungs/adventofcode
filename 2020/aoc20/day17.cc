#include "aoc20/day17.h"

#include <string>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/hash/hash.h"

namespace aoc20 {
namespace day17 {

struct Pos {
  int x;
  int y;
  int z;
  int w;
};

Pos operator+(const Pos& lhs, const Pos& rhs) {
  return {.x = lhs.x + rhs.x,
          .y = lhs.y + rhs.y,
          .z = lhs.z + rhs.z,
          .w = lhs.w + rhs.w};
}

bool operator==(const Pos& lhs, const Pos& rhs) {
  return lhs.x == rhs.x && lhs.y == rhs.y && lhs.z == rhs.z && lhs.w == rhs.w;
}

template <typename H>
H AbslHashValue(H h, const Pos& p) {
  return H::combine(std::move(h), p.x, p.y, p.z, p.w);
}

using LiveCells = absl::flat_hash_set<Pos, absl::Hash<Pos>>;
using Neighbours = absl::flat_hash_map<Pos, std::size_t, absl::Hash<Pos>>;

LiveCells NextGeneration(const LiveCells& cells,
                         const std::vector<Pos>& neighbour_offsets) {
  Neighbours ns;
  for (const Pos& p : cells) {
    // Increment neighbour count for each neighbour by one.
    for (const Pos& no : neighbour_offsets) {
      ++ns[p + no];
    }
  }

  LiveCells nextgen;
  for (const auto& [v, c] : ns) {
    if (c == 3 || (c == 2 && cells.contains(v))) {
      nextgen.insert(v);
    }
  }
  return nextgen;
}

LiveCells RunGameFromInput(const std::vector<std::string>& lines,
                           const std::vector<Pos> neighbour_offsets,
                           std::size_t n_rounds) {
  LiveCells cells;
  for (int y{0}; y < lines.size(); ++y) {
    const std::string& line = lines[y];
    for (int x{0}; x < lines.size(); ++x) {
      if (line[x] == '#') {
        cells.insert(Pos{x, y, 0, 0});
      }
    }
  }

  for (int round{0}; round < n_rounds; ++round) {
    cells = NextGeneration(cells, neighbour_offsets);
  }

  return cells;
}

int Part1(const std::vector<std::string>& lines) {
  std::vector<Pos> neighbour_offsets;
  neighbour_offsets.reserve(26);
  for (int dz : {-1, 0, 1}) {
    for (int dy : {-1, 0, 1}) {
      for (int dx : {-1, 0, 1}) {
        if (dx == 0 && dy == 0 && dz == 0) {
          continue;
        }
        neighbour_offsets.push_back(Pos{dx, dy, dz, 0});
      }
    }
  }

  auto gol = RunGameFromInput(lines, neighbour_offsets, 6);
  return gol.size();
}

int Part2(const std::vector<std::string>& lines) {
  std::vector<Pos> neighbour_offsets;
  neighbour_offsets.reserve(80);
  for (int dw : {-1, 0, 1}) {
    for (int dz : {-1, 0, 1}) {
      for (int dy : {-1, 0, 1}) {
        for (int dx : {-1, 0, 1}) {
          if (dx == 0 && dy == 0 && dz == 0 && dw == 0) {
            continue;
          }
          neighbour_offsets.push_back(Pos{dx, dy, dz, dw});
        }
      }
    }
  }

  auto gol = RunGameFromInput(lines, neighbour_offsets, 6);
  return gol.size();
}

}  // namespace day17
}  // namespace aoc20
