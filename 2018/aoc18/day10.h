#ifndef AOC18_DAY10_H_
#define AOC18_DAY10_H_

#include <algorithm>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

namespace aoc18 {
namespace day10 {

struct Body {
  int x;
  int y;
  int dx;
  int dy;
};

std::optional<Body> ParseBody(const std::string& str);

struct Velocities {
  std::vector<int> dxs;
  std::vector<int> dys;
};

Velocities ExtractVelocities(const std::vector<Body>& bodies);

struct Positions {
  std::vector<int> xs;
  std::vector<int> ys;
};

Positions operator+(const Positions& ps, const Velocities& vs);

std::ostream& operator<<(std::ostream& os, const Positions& ps);

Positions ExtractPositions(const std::vector<Body>& bodies);

struct Rect {
  int x0;
  int y0;
  int x1;
  int y1;

  std::uint64_t width() const { return std::abs(x1 - x0) + 1; }

  std::uint64_t height() const { return std::abs(y1 - y0) + 1; }

  std::uint64_t area() const { return width() * height(); }
};

Rect Bounds(const Positions& ps);

std::optional<Body> ParseBody(const std::string& str);

template <typename ExtractFN>
std::vector<int> ExtractField(const std::vector<Body>& bodies,
                              ExtractFN&& extract) {
  std::vector<int> result;
  result.reserve(bodies.size());
  std::transform(bodies.begin(), bodies.end(), std::back_inserter(result),
                 std::forward<ExtractFN>(extract));
  return result;
}

struct Result {
  Positions configuration;
  int time;
};

Result FindSmallestConfiguration(const std::vector<Body>& bodies);

}  // namespace day10
}  // namespace aoc18

#endif  // AOC18_DAY10_H_
