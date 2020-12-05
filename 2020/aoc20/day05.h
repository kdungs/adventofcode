#ifndef AOC20_DAY05_H_
#define AOC20_DAY05_H_

#include <string>
#include <vector>

namespace aoc20 {
namespace day05 {

constexpr int kNumRows = 128;
constexpr int kNumCols = 8;

constexpr int kRowsBits = 7;
constexpr int kColsBits = 3;

int GetSeatId(const std::string& s);

int Part1(const std::vector<std::string>& lines);

int Part2(const std::vector<std::string>& lines);

int Part2Version2(const std::vector<std::string>& lines);

int Part2Version3(const std::vector<std::string>& lines);

}  // namespace day05
}  // namespace aoc20

#endif  // AOC20_DAY05_H_
