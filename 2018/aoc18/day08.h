#ifndef AOC18_DAY08_H_
#define AOC18_DAY08_H_

#include <vector>

namespace aoc18 {
namespace day08 {

template <typename MetadataVisitorFN>
int Traverse(MetadataVisitorFN&& visit, const std::vector<int>& tree, int pos) {
  int num_children = tree[pos];
  int num_metadata = tree[pos + 1];
  pos = pos + 2;
  for (int child_idx{0}; child_idx < num_children; ++child_idx) {
    pos = Traverse(std::forward<MetadataVisitorFN>(visit), tree, pos);
  }
  for (int meta_idx{0}; meta_idx < num_metadata; ++meta_idx) {
    std::forward<MetadataVisitorFN>(visit)(tree[pos]);
    ++pos;
  }
  return pos;
}

int SumMetadata(const std::vector<int>& tree);

// Part 2
int SumValues(const std::vector<int>& tree);

}  // namespace day08
}  // namespace aoc18

#endif  // AOC18_DAY08_H_
