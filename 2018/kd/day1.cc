#include "kd/day1.h"

#include <iterator>
#include <numeric>
#include <vector>

#include "absl/container/flat_hash_set.h"
#include "absl/types/optional.h"

namespace kd {
namespace day1 {

int CalculateFrequency(const std::vector<int>& increments) {
  return std::accumulate(std::begin(increments), std::end(increments), 0);
}

int FindFirstDuplicateFrequency(const std::vector<int>& increments) {
  absl::flat_hash_set<int> frequencies = {0};
  int current_frequency = 0;
  auto it = increments.begin();
  while (true) {
    if (it == increments.end()) {
      it = increments.begin();
    }

    current_frequency += *it;
    if (frequencies.count(current_frequency) > 0) {
      break;
    }
    frequencies.insert(current_frequency);

    ++it;
  }

  return current_frequency;
}

}  // namespace day1
}  // namespace kd
