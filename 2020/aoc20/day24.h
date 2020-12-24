#ifndef AOC20_DAY24_H_
#define AOC20_DAY24_H_

#include <istream>
#include <vector>

namespace aoc20 {
namespace day24 {

enum class HexDir { NorthWest, NorthEast, East, SouthEast, SouthWest, West };

using HexPath = std::vector<HexDir>;

std::istream& operator>>(std::istream& is, HexDir& d);

std::istream& operator>>(std::istream& is, HexPath& p);

int Part1(const std::vector<HexPath>& paths);

int Part2(const std::vector<HexPath>& paths);

}  // namespace day24
}  // namespace aoc20

#endif  // AOC20_DAY24_H_
