#ifndef KD_DAY2_H_
#define KD_DAY2_H_

#include <string>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/string_view.h"

namespace kd {
namespace day2 {

using LetterCounts = absl::flat_hash_map<char, int>;

LetterCounts CountLetters(const absl::string_view box_id);

bool HasLetterNTimes(const LetterCounts& counts, const int n);

int Checksum(const std::vector<std::string>& box_ids);

int HammingDistance(const absl::string_view lhs, const absl::string_view rhs);

std::string FindCommonLettersOfCorrectBoxIds(
    const std::vector<std::string>& box_ids);

}  // namespace day2
}  // namespace kd

#endif  // KD_DAY2_H_
