#ifndef AOC20_DAY11_H_
#define AOC20_DAY11_H_

#include <optional>
#include <string>
#include <unordered_set>
#include <vector>

#include "aoc20/utils/point.h"

namespace aoc20 {
namespace day11 {

using Seat = utils::Point<int>;
using SeatHash = utils::PointHash<int>;
using SeatPlan = std::unordered_set<Seat, SeatHash>;

std::optional<SeatPlan> ParseSeatPlan(const std::vector<std::string>& lines);

int Part1(const SeatPlan& plan);

int Part2(const SeatPlan& plan);

}  // namespace day11
}  // namespace aoc20

#endif  // AOC20_DAY11_H_
