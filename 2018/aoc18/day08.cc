#include "aoc18/day08.h"

#include <numeric>
#include <vector>

namespace aoc18 {
namespace day08 {

int SumMetadata(const std::vector<int>& tree) {
  int sum = 0;
  Traverse([&sum](int meta) { sum += meta; }, tree, 0);
  return sum;
}

// Part 2
struct Result {
  int pos;
  int sum;
};

Result SumValuesImpl(const std::vector<int>& tree, int pos) {
  int num_children = tree[pos];
  int num_metadata = tree[pos + 1];
  pos = pos + 2;
  std::vector<int> child_values;
  for (int child_idx{0}; child_idx < num_children; ++child_idx) {
    auto res = SumValuesImpl(tree, pos);
    pos = res.pos;
    child_values.push_back(res.sum);
  }
  std::vector<int> metadata;
  for (int meta_idx{0}; meta_idx < num_metadata; ++meta_idx) {
    metadata.push_back(tree[pos]);
    ++pos;
  }

  if (num_children == 0) {
    return Result{.pos = pos,
                  .sum = std::accumulate(metadata.begin(), metadata.end(), 0)};
  }

  int sum{0};
  for (const int meta : metadata) {
    if (meta <= 0 || meta > num_children) {
      continue;
    }
    sum += child_values[meta - 1];
  }

  return Result{.pos = pos, .sum = sum};
}

int SumValues(const std::vector<int>& tree) {
  return SumValuesImpl(tree, 0).sum;
}

}  // namespace day08
}  // namespace aoc18
