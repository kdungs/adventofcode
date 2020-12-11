#include "aoc20/day11.h"

#include <algorithm>
#include <optional>
#include <tuple>
#include <unordered_map>
#include <vector>

namespace aoc20 {
namespace day11 {

namespace {
static const std::vector<std::tuple<int, int>> kDirections{
    {-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}};
}  // namespace

std::optional<SeatPlan> ParseSeatPlan(const std::vector<std::string>& lines) {
  if (lines.empty()) {
    return std::nullopt;
  }
  SeatPlan plan;
  for (int y{0}; y < lines.size(); ++y) {
    const std::string& line = lines[y];
    for (int x{0}; x < line.size(); ++x) {
      if (line[x] == 'L') {
        plan.emplace(x, y);
      }
    }
  }
  return plan;
}

using SeatCount = std::unordered_map<Seat, int, SeatHash>;

SeatCount CountOccupiedNeighbours(const SeatPlan& plan,
                                  const SeatPlan& occupied) {
  SeatCount counts;
  for (const Seat& s : occupied) {
    for (const auto& [dx, dy] : kDirections) {
      Seat n{s.x + dx, s.y + dy};
      // Could filter afterwards.
      if (plan.count(n) > 0) {
        ++counts[n];
      }
    }
  }
  return counts;
}

SeatPlan NextGeneration(const SeatPlan& plan, const SeatPlan& occupied) {
  SeatCount num_occupied_neighbours = CountOccupiedNeighbours(plan, occupied);
  SeatPlan next_gen;
  for (const auto& s : plan) {
    bool occ = occupied.count(s) > 0;
    int numn = num_occupied_neighbours[s];
    if ((occ && numn < 4) || (!occ && numn == 0)) {
      next_gen.insert(s);
    }
  }
  return next_gen;
}

int Part1(const SeatPlan& plan) {
  SeatPlan occupied;
  SeatPlan next_gen = NextGeneration(plan, occupied);
  while (next_gen != occupied) {
    occupied = next_gen;
    next_gen = NextGeneration(plan, occupied);
  }
  return occupied.size();
}

SeatCount CountOccupiedSeatsInDirections(const SeatPlan& plan,
                                         const SeatPlan& occupied) {
  int maxx = std::max_element(
                 plan.begin(), plan.end(),
                 [](const Seat& lhs, const Seat& rhs) { return lhs.x < rhs.x; })
                 ->x;
  int maxy = std::max_element(
                 plan.begin(), plan.end(),
                 [](const Seat& lhs, const Seat& rhs) { return lhs.y < rhs.y; })
                 ->y;

  SeatCount counts;
  for (const Seat& s : plan) {
    for (const auto& [dx, dy] : kDirections) {
      int x = s.x;
      int y = s.y;
      while (x >= 0 && x <= maxx && y >= 0 && y <= maxy) {
        x += dx;
        y += dy;
        Seat n{x, y};
        if (plan.count(n) > 0) {
          if (occupied.count(n) > 0) {
            ++counts[s];
          }
          break;
        }
      }
    }
  }
  return counts;
}

SeatPlan NextGeneration2(const SeatPlan& plan, const SeatPlan& occupied) {
  SeatCount num_occupied_neighbours =
      CountOccupiedSeatsInDirections(plan, occupied);
  SeatPlan next_gen;
  for (const auto& s : plan) {
    bool occ = occupied.count(s) > 0;
    int numn = num_occupied_neighbours[s];
    if ((occ && numn < 5) || (!occ && numn == 0)) {
      next_gen.insert(s);
    }
  }
  return next_gen;
}

int Part2(const SeatPlan& plan) {
  SeatPlan occupied;
  SeatPlan next_gen = NextGeneration2(plan, occupied);
  while (next_gen != occupied) {
    occupied = next_gen;
    next_gen = NextGeneration2(plan, occupied);
  }
  return occupied.size();
}

}  // namespace day11
}  // namespace aoc20
