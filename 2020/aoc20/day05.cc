#include "aoc20/day05.h"

#include <algorithm>
#include <bitset>
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

int Part2Version2(const std::vector<std::string>& lines) {
  constexpr int N = kNumCols * kNumRows;
  std::bitset<N> is_taken;
  int minsid = N;
  int maxsid = 0;
  for (const auto& line : lines) {
    auto sid = GetSeatId(line);
    is_taken.set(sid);
    if (sid < minsid) {
      minsid = sid;
    } else if (sid > maxsid) {
      maxsid = sid;
    }
  }

  for (int sid{minsid + 1}; sid < maxsid; ++sid) {
    if (!is_taken[sid]) {
      return sid;
    }
  }
  return 0;
}

template <std::size_t N>
int findIndexOfHighestBit(const std::bitset<N>& b) {
  constexpr std::size_t BITS = 64;
  constexpr std::size_t NBINS = N / BITS;
  std::array<uint64_t, NBINS>* arr = (std::array<uint64_t, NBINS>*)(&b);

  int idx{0};
  for (int bin{0}; bin < NBINS; ++bin) {
    int64_t x = __builtin_ctzll((*arr)[bin]);
    if (x != 0) {
      idx = bin * BITS + x;
      break;
    }
  }
  return idx;
}

int Part2Version3(const std::vector<std::string>& lines) {
  constexpr int N = kNumCols * kNumRows;
  std::bitset<N> is_taken;
  int minsid = N;
  int maxsid = 0;
  for (const auto& line : lines) {
    auto sid = GetSeatId(line);
    is_taken.set(sid);
    if (sid < minsid) {
      minsid = sid;
    } else if (sid > maxsid) {
      maxsid = sid;
    }
  }

  // Create a mask where all the bits inside the relevant range are set to 1.
  // The two bits at the edges are set to 0 because they are ignored.
  std::bitset<kNumCols * kNumRows> mask;
  mask.set();
  mask >>= minsid;
  mask <<= minsid;
  mask <<= (N - maxsid - 1);
  mask >>= (N - maxsid - 1);

  const auto found = is_taken ^ mask;
  return findIndexOfHighestBit(found);
}

}  // namespace day05
}  // namespace aoc20
