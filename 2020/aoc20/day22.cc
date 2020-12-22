#include "aoc20/day22.h"

#include <algorithm>
#include <cassert>
#include <ios>
#include <istream>
#include <iterator>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include "absl/hash/hash.h"

namespace aoc20 {
namespace day22 {

std::istream& operator>>(std::istream& is, Deck& d) {
  std::copy(std::istream_iterator<int>{is}, std::istream_iterator<int>{},
            std::back_inserter(d));
  return is;
}

std::istream& operator>>(std::istream& is, Game& g) {
  std::string str;
  std::getline(is, str);
  if (str != "Player 1:") {
    is.setstate(std::ios::failbit);
    return is;
  }
  is >> g.p1;
  is.clear();
  std::getline(is, str);
  if (str != "Player 2:") {
    is.setstate(std::ios::failbit);
    return is;
  }
  is >> g.p2;
  return is;
}

struct GameResult {
  bool p1won;
  int score;
};

int CalculateScore(const Deck& d) {
  int score{0};
  int factor{1};
  for (auto it = d.rbegin(); it != d.rend(); ++it) {
    score += *it * factor;
    ++factor;
  }
  return score;
}

GameResult Play(Game g) {
  // Play game.
  while (!g.p1.empty() && !g.p2.empty()) {
    int c1 = g.p1.front();
    g.p1.pop_front();
    int c2 = g.p2.front();
    g.p2.pop_front();
    if (c1 > c2) {
      g.p1.push_back(c1);
      g.p1.push_back(c2);
    } else if (c2 > c1) {
      g.p2.push_back(c2);
      g.p2.push_back(c1);
    } else {
      assert(false);  // apparently all cards are distinct.
    }
  }
  // Determine winner.
  bool p1won = !g.p1.empty();
  Deck* winner = p1won ? &(g.p1) : &(g.p2);
  // Calculate score.
  return GameResult{p1won, CalculateScore(*winner)};
}

int Part1(const Game& g) {
  auto [_, score] = Play(g);
  return score;
}

template <typename H>
H AbslHashValue(H h, const Game& g) {
  return H::combine(std::move(h), g.p1, g.p2);
}

bool operator==(const Game& lhs, const Game& rhs) {
  return lhs.p1 == rhs.p1 && lhs.p2 == rhs.p2;
}

GameResult PlayRecursive(Game g) {
  std::unordered_set<Game, absl::Hash<Game>> states;
  while (!g.p1.empty() && !g.p2.empty()) {
    if (states.count(g) > 0) {
      // We've seen this game state before. P1 wins this sub-game.
      return GameResult{true, CalculateScore(g.p1)};
    }
    states.insert(g);
    int c1 = g.p1.front();
    g.p1.pop_front();
    int c2 = g.p2.front();
    g.p2.pop_front();
    bool p1won = true;
    if (c1 > g.p1.size() || c2 > g.p2.size()) {
      // A players has fewer cards in their deck than their current card's
      // value. The winner of this round is the player with the higher card.
      p1won = c1 > c2;
    } else {
      // Play recursive game with c1 and c2 cards respectively.
      Game sg;
      std::copy(g.p1.begin(), g.p1.begin() + c1, std::back_inserter(sg.p1));
      std::copy(g.p2.begin(), g.p2.begin() + c2, std::back_inserter(sg.p2));
      auto [p1w, _] = PlayRecursive(sg);
      p1won = p1w;
    }
    if (p1won) {
      g.p1.push_back(c1);
      g.p1.push_back(c2);
    } else {
      g.p2.push_back(c2);
      g.p2.push_back(c1);
    }
  }
  // Determine winner of sub-game.
  bool p1won = !g.p1.empty();
  Deck* winner = p1won ? &(g.p1) : &(g.p2);
  return GameResult{p1won, CalculateScore(*winner)};
}

int Part2(const Game& g) {
  auto [_, score] = PlayRecursive(g);
  return score;
}

}  // namespace day22
}  // namespace aoc20
