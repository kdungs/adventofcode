#ifndef AOC18_DAY02_H_
#define AOC18_DAY02_H_

#include <string>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/string_view.h"

namespace aoc18 {
namespace day02 {

using LetterCounts = absl::flat_hash_map<char, int>;

LetterCounts CountLetters(const absl::string_view box_id);

bool HasLetterNTimes(const LetterCounts& counts, const int n);

int Checksum(const std::vector<std::string>& box_ids);

int HammingDistance(const absl::string_view lhs, const absl::string_view rhs);

std::string FindCommonLettersOfCorrectBoxIds(
    const std::vector<std::string>& box_ids);

}  // namespace day02
}  // namespace aoc18

#endif  // AOC18_DAY02_H_
