#include "aoc20/day05.h"

#include <algorithm>
#include <string>
#include <unordered_set>
#include <vector>

namespace aoc20 {
namespace day05 {

int BinaryPartition(int min, int max, char cl, char cr, const std::string& s) {
  int l = min;
  int m = max / 2;
  int r = max;

  for (auto cmd : s) {
    if (cmd == cl) {
      // left half
      r = m;
      m = l + (r - l) / 2;
    } else if (cmd == cr) {
      // right half
      l = m;
      m = l + (r - l) / 2;
    }
  }
  return m;
}

int GetSeatId(const std::string& s) {
  int row = BinaryPartition(0, kNumRows, 'F', 'B', s);
  int col = BinaryPartition(0, kNumCols, 'L', 'R', s);
  return row * kNumCols + col;
}

int Part1(const std::vector<std::string>& lines) {
  int max{0};
  for (const auto& line : lines) {
    auto seat_id = GetSeatId(line);
    if (seat_id > max) {
      max = seat_id;
    }
  }
  return max;
}

int Part2(const std::vector<std::string>& lines) {
  std::unordered_set<int> seat_ids;
  std::transform(lines.begin(), lines.end(),
                 std::inserter(seat_ids, seat_ids.end()), GetSeatId);
  for (int sid{0}; sid < kNumCols * kNumRows; ++sid) {
    if (seat_ids.count(sid) > 0) {
      // Somebody has this seat id!
      continue;
    }
    if (seat_ids.count(sid - 1) == 0 || seat_ids.count(sid + 2) == 0) {
      // Missing seat id
      continue;
    }
    return sid;
  }
  return 0;
}

}  // namespace day05
}  // namespace aoc20
