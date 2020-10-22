#include "kd/day8.h"

#include <vector>

namespace kd {
namespace day8 {

int SumMetadata(const std::vector<int>& tree) {
  int sum = 0;
  Traverse([&sum](int meta) { sum += meta; }, tree, 0);
  return sum;
}

}  // namespace day8
}  // namespace kd
