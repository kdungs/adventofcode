#ifndef AOC20_DAY21_H_
#define AOC20_DAY21_H_

#include <istream>
#include <string>
#include <unordered_set>
#include <vector>

namespace aoc20 {
namespace day21 {

using StrSet = std::unordered_set<std::string>;

struct Entry {
  StrSet ingredients;
  StrSet allergens;
};

std::istream& operator>>(std::istream& is, Entry& e);

int64_t Part1(const std::vector<Entry>& entries);

std::string Part2(const std::vector<Entry>& entries);

}  // namespace day21
}  // namespace aoc20

#endif  // AOC20_DAY21_H_
