#ifndef AOC20_DAY06_H_
#define AOC20_DAY06_H_

#include <bitset>
#include <istream>
#include <vector>

namespace aoc20 {
namespace day06 {

using Answers = std::bitset<26>;
using Block = std::vector<Answers>;

std::vector<Block> ParseInput(std::istream& in);

int Part1(const std::vector<Block>& blocks);

int Part2(const std::vector<Block>& blocks);

}  // namespace day06
}  // namespace aoc20

#endif  // AOC20_DAY06_H_
