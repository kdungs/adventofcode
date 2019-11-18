// The first half can easily be solved using just maths.
// The square of every odd number l defines the bottom right corner of a ring
// with side length l. For a given number n, we can determine l as the next odd
// number larger than sqrt(n). Using the difference d = l^2 - n, we can define
// four quadrants. The half side length is k = l // 2.
//
// Q1: d <= l-1: x = k - d
//               y = -k
// Q2: d <= 2l-2: x = -k
//                y = -k + d - (l-1)
// Q3: d <= 3l-3: x = -k + d - (2l-2)
//                y = k
// Q4: d > 3l - 3: x = k
//                 y = k - d + 3l-3
//
// The number of steps needed under Manhatten distance is s = abs(x) + abs(y).

#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

struct Point {
  int x;
  int y;
};

bool operator==(const Point& lhs, const Point& rhs) {
  return lhs.x == rhs.x && lhs.y == rhs.y;
}

namespace std {
template <>
struct hash<Point> {
  std::size_t operator()(const Point& p) const {
    return std::hash<int>()(p.x) ^ std::hash<int>()(p.y);
  }
};
}  // namespace std

using Map = std::unordered_map<Point, int>;

Point Add(const Point& lhs, const Point& rhs) {
  return {lhs.x + rhs.x, lhs.y + rhs.y};
}

static const Point DIRECTIONS[] = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};

std::vector<Point> Neighbours(const Point& p) {
  std::vector<Point> ns;
  for (const Point& n : DIRECTIONS) {
    ns.push_back(Add(p, n));
  }
  for (const Point& n :
       {Point{1, 1}, Point{1, -1}, Point{-1, 1}, Point{-1, -1}}) {
    ns.push_back(Add(p, n));
  }
  return ns;
}

class SpiralGenerator {
 public:
  const Point& Next() {
    pos_ = Add(pos_, DIRECTIONS[dir_]);
    ++count_;
    if (count_ == level_) {
      dir_ = (dir_ + 1) % 4;
      count_ = 0;
      if (first_) {
        first_ = false;
      } else {
        ++level_;
        first_ = true;
      }
    }
    return pos_;
  }

 private:
  Point pos_ = {0, 0};
  int dir_ = 0;
  int level_ = 1;
  int count_ = 0;
  bool first_ = true;
};

int Solve(const int target) {
  Map m{{Point{0, 0}, 1}};
  SpiralGenerator gen;
  while (true) {
    const Point& p = gen.Next();
    const std::vector<Point> ns = Neighbours(p);
    int value = 0;
    for (const Point& n : ns) {
      const auto it = m.find(n);
      if (it != m.end()) {
        value += it->second;
      }
    }
    if (value > target) {
      return value;
    }
    m[p] = value;
  }
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  int target = std::stoi(argv[1]);
  std::cout << Solve(target) << '\n';
}
