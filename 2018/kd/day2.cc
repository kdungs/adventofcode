#include "kd/day2.h"

#include <algorithm>
#include <functional>
#include <iterator>
#include <numeric>
#include <string>
#include <vector>

#include "absl/strings/string_view.h"

namespace kd {
namespace day2 {

namespace {
int CountOccurences(const int number,
                    const std::vector<LetterCounts>& all_counts) {
  return std::accumulate(
      std::begin(all_counts), std::end(all_counts), 0,
      [number](const int acc, const LetterCounts& counts) {
        return acc + static_cast<int>(HasLetterNTimes(counts, number));
      });
}
}  // namespace

LetterCounts CountLetters(const absl::string_view box_id) {
  LetterCounts counts = {};
  std::for_each(std::begin(box_id), std::end(box_id),
                [&counts](const char c) { ++counts[c]; });
  return counts;
}

bool HasLetterNTimes(const LetterCounts& counts, const int n) {
  for (const auto letter_and_count : counts) {
    const int count = letter_and_count.second;
    if (count == n) {
      return true;
    }
  }
  return false;
}

int Checksum(const std::vector<std::string>& box_ids) {
  std::vector<LetterCounts> all_counts(box_ids.size());
  std::transform(std::begin(box_ids), std::end(box_ids), std::begin(all_counts),
                 CountLetters);

  const int twos = CountOccurences(2, all_counts);
  const int threes = CountOccurences(3, all_counts);

  return twos * threes;
}

namespace {
std::string SharedLetters(const absl::string_view lhs,
                          const absl::string_view rhs) {
  auto lit = std::begin(lhs);
  auto rit = std::begin(rhs);
  while (lit != std::end(lhs) && rit != std::end(rhs)) {
    if (*lit != *rit) {
      return std::string(std::begin(lhs), lit) +
             std::string(std::next(lit), std::end(lhs));
    }
    ++lit;
    ++rit;
  }
  return "";
}

}  // namespace

int HammingDistance(const absl::string_view lhs, const absl::string_view rhs) {
  return std::inner_product(std::begin(lhs), std::end(lhs), std::begin(rhs), 0,
                            std::plus<int>{},
                            [](const char lhs, const char rhs) {
                              return static_cast<int>(lhs != rhs);
                            });
}

std::string FindCommonLettersOfCorrectBoxIds(
    const std::vector<std::string>& box_ids) {
  for (int i = 0; i < box_ids.size() - 1; ++i) {
    const absl::string_view lhs(box_ids[i]);
    for (int k = i + 1; k < box_ids.size(); ++k) {
      const absl::string_view rhs(box_ids[k]);
      if (HammingDistance(lhs, rhs) == 1) {
        return SharedLetters(lhs, rhs);
      }
    }
  }
  return "";
}

}  // namespace day2
}  // namespace kd
