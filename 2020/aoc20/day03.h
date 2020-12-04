#ifndef AOC20_DAY03_H_
#define AOC20_DAY03_H_

#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace aoc20 {
namespace day03 {

class Forest {
 public:
  Forest(int width, int height)
      : w_{width}, h_{height}, is_tree_(width * height, false) {}

  void AddTree(int x, int y) {
    if (x < 0 || x >= w_ || y < 0 || y >= h_) {
      throw std::runtime_error("tree out of bounds");
    }
    is_tree_[idx(x, y)] = true;
  }

  bool IsTree(int x, int y) const {
    // Pattern repeats horizontally.
    x = x % w_;
    return is_tree_[idx(x, y)];
  }

  int width() const { return w_; }

  int height() const { return h_; }

 private:
  int idx(int x, int y) const { return y * w_ + x; }

  int w_;
  int h_;
  std::vector<bool> is_tree_;
};

std::optional<Forest> ParseForest(const std::vector<std::string>& lines);

int Part1(const Forest& forest);

int64_t Part2(const Forest& forest);

}  // namespace day03
}  // namespace aoc20

#endif  // AOC20_DAY03_H_
