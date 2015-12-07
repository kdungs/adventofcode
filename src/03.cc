#include <cassert>
#include <iostream>
#include <functional>

#include <range/v3/all.hpp>
using namespace ranges;

#include "helpers.h"

enum class Direction { NORTH, EAST, SOUTH, WEST };

constexpr Direction direction_from_char(const char c) {
  if (c == '^') { return Direction::NORTH; }
  if (c == '>') { return Direction::EAST; }
  if (c == 'v') { return Direction::SOUTH; }
  if (c == '<') { return Direction::WEST; }
  assert(false);
}

std::ostream& operator<<(std::ostream& os, Direction direction) {
  if (direction == Direction::NORTH) { os << 'N'; }
  else if (direction == Direction::EAST) { os << 'E'; }
  else if (direction == Direction::SOUTH) { os << 'S'; }
  else if (direction == Direction::WEST) { os << 'W'; }
  return os;
}

struct Point {
  int x, y;
};

bool operator<(Point lhs, Point rhs) {
  if (lhs.x == rhs.x) {
    return lhs.y < rhs.y;
  }
  return lhs.x < rhs.x;
}

bool operator==(Point lhs, Point rhs) {
  return lhs.x == rhs.x && lhs.y == rhs.y;
}

bool operator!=(Point lhs, Point rhs) { return !(lhs == rhs); }

std::ostream& operator<<(std::ostream& os, Point p) {
  return os << '(' << p.x << ',' << p.y << ')';
}

Point move_in_direction(const Point& point, Direction direction) {
  if (direction == Direction::NORTH) { return Point{point.x, point.y - 1}; }
  if (direction == Direction::EAST) { return Point{point.x + 1, point.y}; }
  if (direction == Direction::SOUTH) { return Point{point.x, point.y + 1}; }
  if (direction == Direction::WEST) { return Point{point.x - 1, point.y}; }
  assert(false);
};

struct Santa {
  Point position;

  auto operator()(Direction direction) {
    auto p = position;
    position = move_in_direction(position, direction);
    return p;
  }
};

auto count_unique_houses() {
  return make_pipeable([](auto rng) {
    auto houses = rng
                | to_vector
                | action::sort([](auto lhs, auto rhs) { return lhs < rhs; })
                | action::unique;
    return distance(houses);
  });
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }

  auto data = helpers::read_file(argv[1]);
  auto directions = data | view::transform(direction_from_char);


  auto santa = Santa{{0, 0}};
  auto visited_alone = directions
                     | view::transform(santa)
                     | count_unique_houses();

  santa.position = {0, 0};
  auto robot_santa = Santa{{0, 0}};
  auto santas = std::vector<std::reference_wrapper<Santa>>{santa, robot_santa};
  auto visited_split = view::zip_with(
    [](auto dir, auto santa) {
      return santa(dir);
    }, directions, santas | view::cycle) | count_unique_houses();

  std::cout << "Alone: " << visited_alone << '\n';
  std::cout << "Split: " << visited_split << '\n';
}
