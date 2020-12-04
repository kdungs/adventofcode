#ifndef AOC20_DAY04_H_
#define AOC20_DAY04_H_

#include <string>
#include <unordered_map>
#include <vector>

namespace aoc20 {
namespace day04 {

using Passport = std::unordered_map<std::string, std::string>;

std::vector<Passport> ParsePassports(const std::string& input);

int Part1(const std::vector<Passport>& passports);

int Part2(const std::vector<Passport>& passports);

}  // namespace day04
}  // namespace aoc20

#endif  // AOC20_DAY04_H_
