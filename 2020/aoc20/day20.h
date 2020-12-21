#ifndef AOC20_DAY20_H_
#define AOC20_DAY20_H_

#include <algorithm>
#include <array>
#include <bitset>
#include <cassert>
#include <istream>
#include <iterator>
#include <ostream>
#include <string>
#include <vector>

namespace aoc20 {
namespace day20 {

constexpr std::size_t kSize = 10;

// Since std::bitset doesn't have iterators, we cannot use std::reverse on
// them.
template <std::size_t N>
std::bitset<N> reverse(std::bitset<N> bs) {
  for (std::size_t idx{0}; idx < N / 2; ++idx) {
    bool tmp = bs[idx];
    bs[idx] = bs[N - 1 - idx];
    bs[N - 1 - idx] = tmp;
  }
  return bs;
}

using TileBorder = std::bitset<kSize>;

// Since there are only kSize bits in TileBorder there can only be 2^kSize
// distinct borders, so we can store them in another bitset.
using TileBorders = std::bitset<1 << kSize>;

struct Tile {
  uint32_t id;
  std::array<bool, kSize * kSize> data;

  Tile Rotate() const {
    std::array<bool, kSize * kSize> rot;
    for (std::size_t y{0}; y < kSize; ++y) {
      for (std::size_t x{0}; x < kSize; ++x) {
        std::size_t oldx{kSize - 1 - y};
        std::size_t oldy{x};
        rot[y * kSize + x] = data[oldy * kSize + oldx];
      }
    }
    return Tile{id, rot};
  }

  Tile Flip() const {
    Tile flipped{*this};
    for (std::size_t y{0}; y < kSize; ++y) {
      std::reverse(flipped.data.begin() + y * kSize,
                   flipped.data.begin() + (y + 1) * kSize);
    }
    return flipped;
  }

  TileBorder Top() const {
    TileBorder b;
    for (std::size_t x{0}; x < kSize; ++x) {
      b[x] = data[x];
    }
    return b;
  }

  TileBorder Right() const {
    TileBorder b;
    for (std::size_t y{0}; y < kSize; ++y) {
      b[y] = data[(y + 1) * kSize - 1];
    }
    return b;
  }

  TileBorder Bottom() const {
    TileBorder b;
    for (std::size_t x{0}; x < kSize; ++x) {
      b[x] = data[kSize * (kSize - 1) + x];
    }
    return b;
  }

  TileBorder Left() const {
    TileBorder b;
    for (std::size_t y{0}; y < kSize; ++y) {
      b[y] = data[y * kSize];
    }
    return b;
  }

  TileBorder Border(std::size_t direction) const {
    switch (direction) {
      case 0:
        return Top();
      case 1:
        return Right();
      case 2:
        return Bottom();
      case 3:
        return Left();
    }
    assert(false);
  }

  TileBorders Borders() const {
    TileBorders bs;
    bs.set(Top().to_ulong());
    bs.set(Right().to_ulong());
    bs.set(Bottom().to_ulong());
    bs.set(Left().to_ulong());
    return bs;
  }

  // Returns all potential borders we can built through rotation and flipping.
  TileBorders AllBorders() const {
    TileBorders bs = Borders();
    bs.set(reverse(Top()).to_ulong());
    bs.set(reverse(Right()).to_ulong());
    bs.set(reverse(Bottom()).to_ulong());
    bs.set(reverse(Left()).to_ulong());
    return bs;
  }

  std::array<bool, (kSize - 2) * (kSize - 2)> WithoutBorder() const {
    std::array<bool, (kSize - 2) * (kSize - 2)> wob;
    auto it = wob.begin();
    for (std::size_t y{1}; y < kSize - 1; ++y) {
      it = std::copy(data.begin() + y * kSize + 1,
                     data.begin() + y * kSize + kSize - 1, it);
    }
    return wob;
  }

  // Orients this tile such that it can be attached to another tile who has the
  // given border in the given direction. Must be called with a border to which
  // this tile can be attached.
  Tile Orient(std::bitset<kSize> border, std::size_t direction) {
    Tile t{*this};
    std::size_t mydir = (direction + 2) % 4;
    for (int rot{0}; rot < 4; ++rot) {
      if (t.Border(mydir) == border) {
        return t;
      }
      Tile flipped = t.Flip();
      if (flipped.Border(mydir) == border) {
        return flipped;
      }
      t = t.Rotate();
    }
    assert(false);
  }
};

std::istream& operator>>(std::istream& is, Tile& t);

uint64_t Part1(const std::vector<Tile>& tiles);

std::size_t Part2(const std::vector<Tile>& tiles);

}  // namespace day20
}  // namespace aoc20

#endif  // AOC20_DAY20_H_
