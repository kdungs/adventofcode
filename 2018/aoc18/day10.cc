#include "aoc18/day10.h"

#include <algorithm>
#include <optional>
#include <regex>
#include <string>

namespace aoc18 {
namespace day10 {

std::optional<Body> ParseBody(const std::string& str) {
  static const std::regex re{
      R"(^position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>$)"};
  std::smatch match;
  if (!std::regex_match(str, match, re)) {
    return std::nullopt;
  }
  if (match.size() != 5) {
    return std::nullopt;
  }
  try {
    return Body{
        .x = std::stoi(match[1].str()),
        .y = std::stoi(match[2].str()),
        .dx = std::stoi(match[3].str()),
        .dy = std::stoi(match[4].str()),
    };
  } catch (std::exception) {
    return std::nullopt;
  }
}

Velocities ExtractVelocities(const std::vector<Body>& bodies) {
  return Velocities{
      .dxs = ExtractField(bodies, [](const Body& b) { return b.dx; }),
      .dys = ExtractField(bodies, [](const Body& b) { return b.dy; })};
}

Positions operator+(const Positions& ps, const Velocities& vs) {
  Positions new_ps;
  new_ps.xs.reserve(ps.xs.size());
  new_ps.ys.reserve(ps.ys.size());
  std::transform(ps.xs.begin(), ps.xs.end(), vs.dxs.begin(),
                 std::back_inserter(new_ps.xs),
                 [](int x, int dx) { return x + dx; });
  std::transform(ps.ys.begin(), ps.ys.end(), vs.dys.begin(),
                 std::back_inserter(new_ps.ys),
                 [](int y, int dy) { return y + dy; });
  return new_ps;
}

std::ostream& operator<<(std::ostream& os, const Positions& ps) {
  const Rect bounds = Bounds(ps);
  const std::uint64_t w = bounds.width();
  const std::uint64_t h = bounds.height();
  std::vector<char> matrix(w * h, ' ');
  for (int pidx{0}; pidx < ps.xs.size(); ++pidx) {
    const int x = ps.xs[pidx];
    const int y = ps.ys[pidx];
    const int idx = (y - bounds.y0) * w + (x - bounds.x0);
    matrix[idx] = '#';
  }

  for (int y{0}; y < h; ++y) {
    std::copy(matrix.begin() + y * w, matrix.begin() + (y + 1) * w,
              std::ostream_iterator<char>(os, ""));
    os << '\n';
  }
  return os;
}

Positions ExtractPositions(const std::vector<Body>& bodies) {
  return Positions{
      .xs = ExtractField(bodies, [](const Body& b) { return b.x; }),
      .ys = ExtractField(bodies, [](const Body& b) { return b.y; })};
}

Rect Bounds(const Positions& ps) {
  return Rect{
      .x0 = *std::min_element(ps.xs.begin(), ps.xs.end()),
      .y0 = *std::min_element(ps.ys.begin(), ps.ys.end()),
      .x1 = *std::max_element(ps.xs.begin(), ps.xs.end()),
      .y1 = *std::max_element(ps.ys.begin(), ps.ys.end()),
  };
}

Result FindSmallestConfiguration(const std::vector<Body>& bodies) {
  Positions ps = ExtractPositions(bodies);
  Velocities vs = ExtractVelocities(bodies);
  std::uint64_t area = Bounds(ps).area();
  Positions next_ps = ps + vs;
  std::uint64_t next_area = Bounds(next_ps).area();
  int t = 0;
  while (next_area < area) {
    ++t;
    ps = next_ps;
    area = next_area;
    next_ps = ps + vs;
    next_area = Bounds(next_ps).area();
  }
  return Result{.configuration = ps, .time = t};
}

}  // namespace day10
}  // namespace aoc18
