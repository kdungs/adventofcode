#ifndef AOC20_DAY13_H_
#define AOC20_DAY13_H_

#include <string>
#include <unordered_map>

namespace aoc20 {
namespace day13 {

std::unordered_map<int, int> ParseBusses(const std::string& line);

int Part1(int target, const std::unordered_map<int, int>& busses);

int64_t Part2(const std::unordered_map<int, int>& busses);

}  // namespace day13
}  // namespace aoc20

#endif  // AOC20_DAY13_H_
