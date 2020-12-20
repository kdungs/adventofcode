#include "aoc20/day20.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <stack>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "aoc20/utils/point.h"

namespace aoc20 {
namespace day20 {

std::unordered_map<uint32_t, int> CountCandidates(const std::vector<Tile>& ts) {
  // Prepare all border configurations for faster lookup.
  // This vector is aligned with ts.
  std::vector<TileBorders> base_borders;
  base_borders.reserve(ts.size());
  std::transform(ts.begin(), ts.end(), std::back_inserter(base_borders),
                 [](const Tile& t) { return t.Borders(); });

  std::unordered_map<uint32_t, int> cnts;
  for (std::size_t idx{0}; idx < ts.size(); ++idx) {
    const Tile& t = ts[idx];
    TileBorders all = t.AllBorders();
    for (std::size_t j{idx + 1}; j < ts.size(); ++j) {
      if ((all & base_borders[j]).any()) {
        // There is overlap between the possible borders of the current tile
        // and the other one.
        ++cnts[t.id];
        ++cnts[ts[j].id];
      }
    }
  }
  return cnts;
}

uint64_t Part1(const std::vector<Tile>& tiles) {
  // For some reason, there seems to be only one valid configuration of the
  // input, so all we need to do is to find the _four_ tiles which can have
  // only 2 neighbours; those are the corner pieces and multiply their ids.
  auto cands = CountCandidates(tiles);
  uint64_t result{1};
  for (const auto& [k, v] : cands) {
    if (v == 2) {
      result *= k;
    }
  }
  return result;
}

// ------
// Part 2
// ------

using Pos = utils::Point<int>;
using PosHash = utils::PointHash<int>;
using Puzzle = std::unordered_map<Pos, Tile, PosHash>;

static const std::vector<Pos> kDirections{
    Pos{0, -1},  // up
    Pos{1, 0},   // right
    Pos{0, 1},   // down
    Pos{-1, 0}   // left
};

struct Image {
  std::size_t size;  // Image is square.
  std::vector<bool> pixels;

  bool Get(int x, int y) const { return pixels[y * size + x]; }

  void Set(int x, int y, bool v) { pixels[y * size + x] = v; }

  Image Rotate() const {
    std::vector<bool> rot(pixels.size(), false);
    for (std::size_t y{0}; y < size; ++y) {
      for (std::size_t x{0}; x < size; ++x) {
        const std::size_t oldx{size - 1 - y};
        const std::size_t oldy{x};
        rot[y * size + x] = pixels[oldy * size + oldx];
      }
    }
    return Image{.size = size, .pixels = rot};
  }

  Image Flip() const {
    std::vector<bool> flipped(pixels.begin(), pixels.end());
    for (std::size_t y{0}; y < size; ++y) {
      std::reverse(flipped.begin() + y * size,
                   flipped.begin() + (y + 1) * size);
    }
    return Image{.size = size, .pixels = flipped};
  }
};

std::ostream& operator<<(std::ostream& os, const Image& img) {
  constexpr char one[] = "█";
  constexpr char zero[] = " ";

  for (int y{0}; y < img.size; ++y) {
    for (int x{0}; x < img.size; ++x) {
      os << (img.Get(x, y) ? one : zero);
    }
    os << '\n';
  }
  return os;
}

Image AssembleImage(const std::vector<Tile>& tiles) {
  // Since the borders seem to have exactly a 1:1 correspondence, we can
  // greedily build our puzzle. Start with any one and do a flood fill.
  //
  // Tiles in puzzle are oriented correctly. For each correctly oriented
  // piece, we can find its neighbours and derive their orientation.
  const Pos startpos{0, 0};
  const Tile& start = tiles[0];
  Puzzle puzzle;
  std::unordered_set<uint32_t> used_tiles{start.id};
  puzzle[startpos] = start;
  std::stack<Pos> next;
  next.push(Pos{0, 0});

  while (!next.empty()) {
    Pos pos = next.top();
    next.pop();
    const Tile& cur = puzzle[pos];
    const TileBorders borders = cur.Borders();

    // Find potential neighbours in all directions; if we can't find any in one
    // or two directions, that's okay because the piece might be on the edge or
    // even in a corner or we might have already found pieces in all
    // directions.
    for (Tile other : tiles) {
      if (used_tiles.count(other.id) > 0) {
        continue;
      }
      // If the current tile does not have any intersecting edge with the other
      // one, the other one is not interesting.
      // Here, we could sanity check that the and only has a single bit set.
      auto intersection = borders & other.AllBorders();
      if (!intersection.any()) {
        continue;
      }

      // 0 = up, 1 = right, 2 = down, 3 = left
      for (std::size_t bidx{0}; bidx < 4; ++bidx) {
        TileBorder border = cur.Border(bidx);
        if (!intersection.test(border.to_ulong())) {
          continue;
        }
        Pos npos = pos + kDirections[bidx];
        Tile ntile = other.Orient(border, bidx);

        puzzle[npos] = ntile;
        used_tiles.insert(ntile.id);
        next.push(npos);
        break;  // other can only match in one direction
      }
    }
  }

  // After this, we should have a complete picture. Now, we need to assemble
  // the picture, stripping away the borders. Since we started with an
  // arbitrary puzzle piece, need to translate the puzzle so its upper right
  // corner has the coordinate (0, 0).
  int minx{std::numeric_limits<int>::max()};
  int miny{std::numeric_limits<int>::max()};
  for (const auto& [k, v] : puzzle) {
    if (k.x < minx) {
      minx = k.x;
    }
    if (k.y < miny) {
      miny = k.y;
    }
  }
  const std::size_t n = std::sqrt(tiles.size());
  const std::size_t k = kSize - 2;
  const std::size_t nk = n * k;

  Image image{.size = nk, .pixels = std::vector<bool>(nk * nk, false)};
  const auto Stitch = [&image](const Tile& t, const Pos& p) {
    auto img = t.WithoutBorder();
    for (int dy{0}; dy < k; ++dy) {
      const int y = p.y * k + dy;
      for (int dx{0}; dx < k; ++dx) {
        const int x = p.x * k + dx;
        image.Set(x, y, img[dy * k + dx]);
      }
    }
  };

  for (int dy{0}; dy < n; ++dy) {
    for (int dx{0}; dx < n; ++dx) {
      Pos p{minx + dx, miny + dy};
      const Tile& t = puzzle[p];
      Stitch(t, Pos{dx, dy});
    }
  }

  return image;
}

bool IsSeaMonster(const Image& img, const Pos& p) {
  //  01234567890123456789
  // 0                  #
  // 1#    ##    ##    ###
  // 2 #  #  #  #  #  #
  const std::vector<Pos> monsterpos{
      {p.x + 18, p.y + 0}, {p.x + 0, p.y + 1},  {p.x + 5, p.y + 1},
      {p.x + 6, p.y + 1},  {p.x + 11, p.y + 1}, {p.x + 12, p.y + 1},
      {p.x + 17, p.y + 1}, {p.x + 18, p.y + 1}, {p.x + 19, p.y + 1},
      {p.x + 1, p.y + 2},  {p.x + 4, p.y + 2},  {p.x + 7, p.y + 2},
      {p.x + 10, p.y + 2}, {p.x + 13, p.y + 2}, {p.x + 16, p.y + 2}};
  for (auto mpos : monsterpos) {
    if (mpos.x >= img.size || mpos.y >= img.size) {
      return false;
    }
    if (!img.Get(mpos.x, mpos.y)) {
      return false;
    }
  }
  return true;
}

int CountSeaMonsters(const Image& img) {
  int count{0};
  for (int y{0}; y < img.size; ++y) {
    for (int x{0}; x < img.size; ++x) {
      if (IsSeaMonster(img, Pos{x, y})) {
        ++count;
      }
    }
  }
  return count;
}

// Roate and flip image until we find sea monsters and return their count.
int FindSeaMonsters(Image img) {
  for (int rot{0}; rot < 4; ++rot) {
    int num = CountSeaMonsters(img);
    if (num > 0) {
      return num;
    }
    num = CountSeaMonsters(img.Flip());
    if (num > 0) {
      return num;
    }
    img = img.Rotate();
  }
  return 0;
}

std::size_t Part2(const std::vector<Tile>& tiles) {
  Image img = AssembleImage(tiles);
  int cnt = FindSeaMonsters(img);
  const int smsize = 15;
  return std::count(img.pixels.begin(), img.pixels.end(), true) - cnt * smsize;
}

}  // namespace day20
}  // namespace aoc20
