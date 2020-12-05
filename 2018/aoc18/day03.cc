#include "aoc18/day03.h"

#include <algorithm>
#include <iterator>
#include <regex>
#include <string>

#include "absl/types/optional.h"

namespace aoc18 {
namespace day03 {

absl::optional<Rectangle> ParseRectangle(const std::string& str) {
  std::regex re("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)");
  std::smatch sm;
  if (!std::regex_match(str, sm, re) || sm.size() != 6) {
    return absl::nullopt;
  }

  return Rectangle(std::stoi(sm[1]), std::stoi(sm[2]), std::stoi(sm[3]),
                   std::stoi(sm[4]), std::stoi(sm[5]));
}

GridState operator+(const GridState lhs, const GridState rhs) {
  if (lhs == GridState::kEmpty) {
    return rhs;
  }
  if (rhs == GridState::kEmpty) {
    return lhs;
  }
  return GridState::kMore;
}

GridState& operator+=(GridState& lhs, const GridState rhs) {
  lhs = lhs + rhs;
  return lhs;
}

Grid& Grid::AddRectangle(const Rectangle& rect) {
  for (int dy = 0; dy < rect.h(); ++dy) {
    const int y = rect.y() + dy;
    for (int dx = 0; dx < rect.w(); ++dx) {
      const int x = rect.x() + dx;
      states_[Index(x, y)] += GridState::kOne;
    }
  }
  return *this;
}

int Grid::OverlapArea() const {
  return std::count_if(
      std::begin(states_), std::end(states_),
      [](const GridState gs) { return gs == GridState::kMore; });
}

bool Grid::DoesOverlap(const Rectangle& rect) const {
  for (int dy = 0; dy < rect.h(); ++dy) {
    const int y = rect.y() + dy;
    for (int dx = 0; dx < rect.w(); ++dx) {
      const int x = rect.x() + dx;
      if (states_[Index(x, y)] == GridState::kMore) {
        return true;
      }
    }
  }
  return false;
}

absl::optional<Rectangle> Grid::FindNonOverlapping(
    const std::vector<Rectangle>& claims) const {
  for (const Rectangle& claim : claims) {
    if (!DoesOverlap(claim)) {
      return claim;
    }
  }
  return absl::nullopt;
}

/*static*/ Grid Grid::Build(const std::vector<Rectangle>& claims) {
  std::vector<int> corner_xs(claims.size());
  std::vector<int> corner_ys(claims.size());
  std::transform(std::begin(claims), std::end(claims), std::begin(corner_xs),
                 [](const Rectangle& r) { return r.x() + r.w(); });
  std::transform(std::begin(claims), std::end(claims), std::begin(corner_ys),
                 [](const Rectangle& r) { return r.y() + r.h(); });

  const int w = *std::max_element(std::begin(corner_xs), std::end(corner_xs));
  const int h = *std::max_element(std::begin(corner_ys), std::end(corner_ys));

  Grid g(w, h);
  for (const Rectangle& claim : claims) {
    g.AddRectangle(claim);
  }
  return g;
}

}  // namespace day03
}  // namespace aoc18
