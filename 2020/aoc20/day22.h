#ifndef AOC20_DAY22_H_
#define AOC20_DAY22_H_

#include <deque>
#include <istream>

namespace aoc20 {
namespace day22 {

using Deck = std::deque<int>;

struct Game {
  Deck p1;
  Deck p2;
};

std::istream& operator>>(std::istream& is, Game& g);

int Part1(const Game& g);

int Part2(const Game& g);

}  // namespace day22
}  // namespace aoc20

#endif  // AOC20_DAY22_H_
