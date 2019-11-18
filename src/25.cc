#include <iostream>

struct Coordinate {
  unsigned row, col;
};

bool operator==(const Coordinate lhs, const Coordinate rhs) {
  return (lhs.row == rhs.row && lhs.col == rhs.col);
}

bool operator!=(const Coordinate lhs, const Coordinate rhs) {
  return !(lhs == rhs);
}

std::ostream& operator<<(std::ostream& os, const Coordinate c) {
  return (os << '(' << c.row << ", " << c.col << ')');
}

Coordinate& operator++(Coordinate& c) {
  if (c.row == 1) {
    c = {c.col + 1, 1};
  } else {
    c = {c.row - 1, c.col + 1};
  }
  return c;
}

template <unsigned long first = 20151125, unsigned long factor = 252533,
          unsigned long cutoff = 33554393>
class CodeGenerator {
 public:
  const auto& code() const noexcept { return code_; }
  const auto& coordinate() const noexcept { return coord_; }

  auto& operator++() {
    code_ = (code_ * factor) % cutoff;
    ++coord_;
    return *this;
  }

 private:
  unsigned long code_ = first;
  Coordinate coord_ = {1, 1};
};

int main() {
  auto cg = CodeGenerator<>{};
  while (cg.coordinate() != Coordinate{2978, 3083}) {
    ++cg;
  }
  std::cout << cg.code() << '\n';
}
