#include <cassert>
#include <iostream>

#include <range/v3/all.hpp>

#include "helpers.h"

enum class Direction { NORTH, EAST, SOUTH, WEST };

constexpr Direction direction_from_char(const char c) {
  if (c == '^') {
    return Direction::NORTH;
  }
  if (c == '>') {
    return Direction::EAST;
  }
  if (c == 'v') {
    return Direction::SOUTH;
  }
  if (c == '<') {
    return Direction::WEST;
  }
  assert(false);
}

std::ostream& operator<<(std::ostream& os, Direction direction) {
  if (direction == Direction::NORTH) {
    os << 'N';
  } else if (direction == Direction::EAST) {
    os << 'E';
  } else if (direction == Direction::SOUTH) {
    os << 'S';
  } else if (direction == Direction::WEST) {
    os << 'W';
  }
  return os;
}

struct Point {
  int x, y;
};

bool operator==(Point lhs, Point rhs) {
  return lhs.x == rhs.x && lhs.y == rhs.y;
}

bool operator!=(Point lhs, Point rhs) { return !(lhs == rhs); }

std::ostream& operator<<(std::ostream& os, Point p) {
  return os << '(' << p.x << ',' << p.y << ')';
}

Point move_in_direction(const Point& point, Direction direction) {
  if (direction == Direction::NORTH) {
    return Point{point.x, point.y - 1};
  }
  if (direction == Direction::EAST) {
    return Point{point.x + 1, point.y};
  }
  if (direction == Direction::SOUTH) {
    return Point{point.x, point.y + 1};
  }
  if (direction == Direction::WEST) {
    return Point{point.x - 1, point.y};
  }
  assert(false);
};

int main(int argc, char* argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  using namespace ranges;

  auto data = helpers::read_file(argv[1]);
  auto current_point = Point{0, 0};
  auto houses = data | view::transform(direction_from_char) |
                view::transform([&current_point](auto d) mutable {
                  auto p = current_point;
                  current_point = move_in_direction(current_point, d);
                  return p;
                }) |
                to_vector | action::sort([](auto lhs, auto rhs) {
                  if (lhs.x == rhs.x) {
                    return lhs.y < rhs.y;
                  }
                  return lhs.x < rhs.x;
                }) |
                action::unique;
  auto n_houses = distance(houses);

  std::cout << "Individual houses: " << n_houses << '\n';
}
