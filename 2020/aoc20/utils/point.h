#ifndef AOC20_UTILS_POINT_H_
#define AOC20_UTILS_POINT_H_

#include <functional>

namespace aoc20 {
namespace utils {

template <typename T>
struct Point {
  Point(T x, T y) : x{x}, y{y} {}

  T x;
  T y;
};

template <typename T>
bool operator==(const Point<T>& lhs, const Point<T>& rhs) {
  return lhs.x == rhs.x && lhs.y == rhs.y;
}

template <typename T>
bool operator!=(const Point<T>& lhs, const Point<T>& rhs) {
  return !(lhs == rhs);
}

template <typename T>
Point<T> operator+(const Point<T>& lhs, const Point<T>& rhs) {
  return Point{
    .x = lhs.x + rhs.x,
    .y = lhs.y + rhs.y

  };
}

template <typename T>
struct PointHash {
  std::size_t operator()(const Point<T>& p) const {
    auto hasher = std::hash<T>{};
    std::size_t hash = hasher(p.x);
    hash ^= hasher(p.y) + 0x9e3779b9 + (hash << 6) + (hash >> 2);
    return hash;
  }
};

}  // namespace utils
}  // namespace aoc20


namespace std {
template <typename T>
class hash<aoc20::utils::Point<T>> : aoc20::utils::PointHash<T> {};
}

#endif  // AOC20_UTILS_POINT_H_
