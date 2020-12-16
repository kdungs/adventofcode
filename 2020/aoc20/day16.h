#ifndef AOC20_DAY16_H_
#define AOC20_DAY16_H_

#include <optional>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

namespace aoc20 {
namespace day16 {

struct Range {
  int from;
  int to;

  bool IsValid(int num) const { return from <= num && num <= to; }
};

struct Or {
  Range lhs;
  Range rhs;

  bool IsValid(int num) const { return lhs.IsValid(num) || rhs.IsValid(num); }
};

using Ticket = std::vector<int>;
using Ranges = std::vector<std::tuple<std::string, Or>>;

struct Input {
  Ranges ranges;
  Ticket yours;
  std::vector<Ticket> others;
};

std::optional<Input> ParseInput(const std::string& in);

int Part1(const Input& in);

std::unordered_map<std::string, std::size_t> DeterminePositions(
    const Input& in);

uint64_t Part2(const Input& in);

}  // namespace day16
}  // namespace aoc20

#endif  // AOC20_DAY16_H_
