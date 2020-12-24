#include "aoc20/day24.h"

#include <istream>
#include <numeric>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "aoc20/utils/point.h"

namespace aoc20 {
namespace day24 {

std::istream& operator>>(std::istream& is, HexDir& d) {
  char c1;
  is.get(c1);
  if (c1 == '\n') {
    return is;
  }
  if (c1 == 'e') {
    d = HexDir::East;
  } else if (c1 == 'w') {
    d = HexDir::West;
  } else {
    char c2;
    is >> c2;
    if (c1 == 'n') {
      if (c2 == 'e') {
        d = HexDir::NorthEast;
      }
      if (c2 == 'w') {
        d = HexDir::NorthWest;
      }
    } else if (c1 == 's') {
      if (c2 == 'e') {
        d = HexDir::SouthEast;
      }
      if (c2 == 'w') {
        d = HexDir::SouthWest;
      }
    }
  }
  return is;
}

std::istream& operator>>(std::istream& is, HexPath& p) {
  std::string line;
  std::getline(is, line);
  std::stringstream ss{line};
  HexDir d;
  while (ss >> d) {
    p.push_back(d);
  }
  return is;
}

using HexVec = utils::Point<int>;

static const std::unordered_map<HexDir, HexVec> kDirectionVectors{
    {HexDir::NorthWest, HexVec{0, -1}}, {HexDir::NorthEast, HexVec{1, -1}},
    {HexDir::East, HexVec{1, 0}},       {HexDir::SouthEast, HexVec{0, 1}},
    {HexDir::SouthWest, HexVec{-1, 1}}, {HexDir::West, HexVec{-1, 0}}};

HexVec Walk(const HexPath& p) {
  return std::accumulate(p.begin(), p.end(), HexVec{0, 0},
                         [](const HexVec& acc, HexDir d) {
                           return acc + kDirectionVectors.at(d);
                         });
}

std::unordered_set<HexVec> InitialPattern(const std::vector<HexPath>& paths) {
  std::unordered_map<HexVec, bool> black;
  for (const HexPath& p : paths) {
    auto v = Walk(p);
    black[v] ^= true;
  }

  std::unordered_set<HexVec> pattern;
  for (const auto& [v, b] : black) {
    if (b) {
      pattern.insert(v);
    }
  }
  return pattern;
}

int Part1(const std::vector<HexPath>& paths) {
  return InitialPattern(paths).size();
}

std::unordered_set<HexVec> NextGeneration(
    const std::unordered_set<HexVec>& black) {
  std::unordered_map<HexVec, int> neighbours;
  for (const HexVec& v : black) {
    for (const auto& [_, dir] : kDirectionVectors) {
      ++neighbours[v + dir];
    }
  }

  std::unordered_set<HexVec> next_gen;
  for (const auto& [v, c] : neighbours) {
    bool is_black = black.count(v) > 0;
    if (c == 2 || (is_black && c == 1)) {
      next_gen.insert(v);
    }
  }
  return next_gen;
}

int Part2(const std::vector<HexPath>& paths) {
  constexpr int kRounds = 100;
  auto gen = InitialPattern(paths);
  for (int round{0}; round < kRounds; ++round) {
    gen = NextGeneration(gen);
  }
  return gen.size();
}

}  // namespace day24
}  // namespace aoc20
