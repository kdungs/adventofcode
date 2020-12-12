#include "aoc20/day12.h"

#include <array>
#include <istream>

#include "aoc20/utils/point.h"

namespace aoc20 {
namespace day12 {

using Pos = utils::Point<int>;

int Part1(std::istream& in) {
  std::array<Pos, 4> directions{
      Pos{0, 1},   // North
      Pos{1, 0},   // East
      Pos{0, -1},  // South
      Pos{-1, 0}   // West
  };
  Pos pos(0, 0);
  int dir{1};  // start facing east

  char action;
  int amount;
  while (in >> action >> amount) {
    switch (action) {
      case 'N':
        pos = pos + amount * directions[0];
        break;
      case 'E':
        pos = pos + amount * directions[1];
        break;
      case 'S':
        pos = pos + amount * directions[2];
        break;
      case 'W':
        pos = pos + amount * directions[3];
        break;
      case 'F':
        pos = pos + amount * directions[dir];
        break;
      case 'L':
        dir = (dir + 4 - amount / 90) % 4;
        break;
      case 'R':
        dir = (dir + amount / 90) % 4;
        break;
      default:
        break;
    }
  }
  return std::abs(pos.x) + std::abs(pos.y);
}

int Part2(std::istream& in) {
  Pos ship(0, 0);
  Pos waypoint(10, 1);

  char action;
  int amount;
  while (in >> action >> amount) {
    switch (action) {
      case 'N':
        waypoint = waypoint + amount * Pos{0, 1};
        break;
      case 'E':
        waypoint = waypoint + amount * Pos{1, 0};
        break;
      case 'S':
        waypoint = waypoint + amount * Pos{0, -1};
        break;
      case 'W':
        waypoint = waypoint + amount * Pos{-1, 0};
        break;
      case 'F':
        ship = ship + amount * waypoint;
        break;
      case 'L':
        for (int i{0}; i < amount / 90; ++i) {
          waypoint = Pos(-waypoint.y, waypoint.x);
        }
        break;
      case 'R':
        for (int i{0}; i < amount / 90; ++i) {
          waypoint = Pos(waypoint.y, -waypoint.x);
        }
        break;
      default:
        break;
    }
  }
  return std::abs(ship.x) + std::abs(ship.y);
}

}  // namespace day12
}  // namespace aoc20
