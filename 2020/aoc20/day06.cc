#include "aoc20/day06.h"

#include <bitset>
#include <functional>
#include <istream>
#include <numeric>
#include <string>
#include <vector>

namespace aoc20 {
namespace day06 {

using Answers = std::bitset<26>;
using Block = std::vector<Answers>;

std::vector<Block> ParseInput(std::istream& in) {
  std::vector<Block> blocks;
  Block cur;
  std::string line;
  while (std::getline(in, line)) {
    if (line == "") {
      // End of block.
      blocks.push_back(cur);
      cur.clear();
      continue;
    }
    Answers a;
    for (char c : line) {
      a.set(c - 'a');
    }
    cur.push_back(a);
  }
  blocks.push_back(cur);
  return blocks;
}

int Part1(const std::vector<Block>& blocks) {
  int sum{0};
  for (const auto& block : blocks) {
    sum += std::reduce(block.begin() + 1, block.end(), block[0],
                       std::bit_or<Answers>())
               .count();
  }
  return sum;
}

int Part2(const std::vector<Block>& blocks) {
  int sum{0};
  for (const auto& block : blocks) {
    sum += std::reduce(block.begin() + 1, block.end(), block[0],
                       std::bit_and<Answers>())
               .count();
  }
  return sum;
}

}  // namespace day06
}  // namespace aoc20
