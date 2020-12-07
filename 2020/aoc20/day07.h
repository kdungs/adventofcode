#ifndef AOC20_DAY07_H_
#define AOC20_DAY07_H_

#include <istream>
#include <optional>
#include <string>
#include <unordered_map>

namespace aoc20 {
namespace day07 {

using Color = std::string;
using Count = std::unordered_map<Color, std::size_t>;
using Rules = std::unordered_map<Color, Count>;

std::optional<Rules> ParseRules(std::istream& in);

int Part1(const Rules& rules);

int Part2(const Rules& rules);

}  // namespace day07
}  // namespace aoc20

#endif  // AOC20_DAY07_H_
