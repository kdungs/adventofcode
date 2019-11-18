#ifndef KD_DAY3_H_
#define KD_DAY3_H_

#include <string>
#include <vector>

#include "absl/types/optional.h"

namespace kd {
namespace day3 {

class Rectangle {
 public:
  Rectangle(int id, int x, int y, int w, int h)
      : id_(id), x_(x), y_(y), w_(w), h_(h) {}

  int id() const { return id_; }
  int x() const { return x_; }
  int y() const { return y_; }
  int w() const { return w_; }
  int h() const { return h_; }

 private:
  int id_;
  int x_;
  int y_;
  int w_;
  int h_;
};

absl::optional<Rectangle> ParseRectangle(const std::string& str);

enum class GridState { kEmpty, kOne, kMore };

GridState operator+(const GridState lhs, const GridState rhs);
GridState& operator+=(GridState& lhs, const GridState rhs);

class Grid {
 public:
  Grid(const int width, const int height)
      : width_(width), states_(width * height) {}

  Grid& AddRectangle(const Rectangle& rect);
  int OverlapArea() const;

  bool DoesOverlap(const Rectangle& rect) const;
  absl::optional<Rectangle> FindNonOverlapping(
      const std::vector<Rectangle>& claims) const;

  static Grid Build(const std::vector<Rectangle>& claims);

 private:
  int Index(const int x, const int y) const { return y * width_ + x; }

  const int width_;
  std::vector<GridState> states_;
};

}  // namespace day3
}  // namespace kd

#endif  // KD_DAY3_H_
