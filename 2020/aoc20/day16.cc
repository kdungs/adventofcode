#include "aoc20/day16.h"

#include <algorithm>
#include <numeric>
#include <optional>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

#include "absl/strings/match.h"
#include "absl/strings/numbers.h"
#include "absl/strings/str_split.h"
#include "absl/strings/string_view.h"

namespace aoc20 {
namespace day16 {

std::optional<Range> ParseRange(absl::string_view s) {
  std::vector<absl::string_view> parts = absl::StrSplit(s, '-');
  if (parts.size() != 2) {
    return std::nullopt;
  }
  Range r;
  if (!absl::SimpleAtoi(parts[0], &r.from)) {
    return std::nullopt;
  }
  if (!absl::SimpleAtoi(parts[1], &r.to)) {
    return std::nullopt;
  }
  return r;
}

std::optional<Ranges> ParseRanges(absl::string_view s) {
  Ranges ranges;
  for (absl::string_view line : absl::StrSplit(s, '\n')) {
    std::vector<absl::string_view> parts = absl::StrSplit(line, ": ");
    if (parts.size() != 2) {
      return std::nullopt;
    }
    std::vector<absl::string_view> rparts = absl::StrSplit(parts[1], " or ");
    if (rparts.size() != 2) {
      return std::nullopt;
    }
    auto maybe_lhs = ParseRange(rparts[0]);
    if (!maybe_lhs.has_value()) {
      return std::nullopt;
    }
    auto maybe_rhs = ParseRange(rparts[1]);
    if (!maybe_rhs.has_value()) {
      return std::nullopt;
    }
    ranges.push_back(std::tuple<std::string, Or>{
        parts[0], Or{maybe_lhs.value(), maybe_rhs.value()}});
  }
  return ranges;
}

std::optional<Ticket> ParseTicket(absl::string_view s) {
  Ticket ticket;
  for (absl::string_view numstr : absl::StrSplit(s, ',')) {
    int num{0};
    if (!absl::SimpleAtoi(numstr, &num)) {
      return std::nullopt;
    }
    ticket.push_back(num);
  }
  return ticket;
}

std::optional<Input> ParseInput(const std::string& in) {
  std::vector<std::string_view> parts = absl::StrSplit(in, "\n\n");
  if (parts.size() != 3) {
    return std::nullopt;
  }

  auto maybe_ranges = ParseRanges(parts[0]);
  if (!maybe_ranges.has_value()) {
    return std::nullopt;
  }

  std::vector<std::string_view> your_parts = absl::StrSplit(parts[1], '\n');
  if (your_parts.size() != 2) {
    return std::nullopt;
  }
  auto maybe_yours = ParseTicket(your_parts[1]);
  if (!maybe_yours.has_value()) {
    return std::nullopt;
  }

  std::vector<std::string_view> other_parts = absl::StrSplit(parts[2], '\n');
  if (other_parts.size() < 2) {
    return std::nullopt;
  }
  std::vector<Ticket> others;
  for (int idx{1}; idx < other_parts.size() - 1; ++idx) {  // newline before EOF
    auto maybe_numbers = ParseTicket(other_parts[idx]);
    if (!maybe_numbers.has_value()) {
      return std::nullopt;
    }
    others.push_back(maybe_numbers.value());
  }

  return Input{.ranges = maybe_ranges.value(),
               .yours = maybe_yours.value(),
               .others = others};
}

bool IsNumValid(const Ranges& rs, int num) {
  for (const auto& [_, r] : rs) {
    if (r.IsValid(num)) {
      return true;
    }
  }
  return false;
}

int SumInvalidNumbers(const Ranges& rs, const Ticket& t) {
  int count{0};
  for (int num : t) {
    if (!IsNumValid(rs, num)) {
      count += num;
    }
  }
  return count;
}

int Part1(const Input& in) {
  int sum{0};
  for (const Ticket& t : in.others) {
    sum += SumInvalidNumbers(in.ranges, t);
  }
  return sum;
}

std::unordered_map<std::string, std::size_t> DeterminePositions(
    const Input& in) {
  // Filter valid tickets.
  std::vector<Ticket> valid_tickets;
  valid_tickets.reserve(in.others.size());
  std::copy_if(in.others.begin(), in.others.end(),
               std::back_inserter(valid_tickets), [&in](const Ticket& t) {
                 for (int num : t) {
                   if (!IsNumValid(in.ranges, num)) {
                     return false;
                   }
                 }
                 return true;
               });
  valid_tickets.shrink_to_fit();

  std::size_t num_keys = in.ranges.size();
  if (num_keys > 64) {
    throw std::runtime_error("assumption max 64 keys violated");
  }
  // At the start, each range is a candidate for each position. Then we iterate
  // over all tickets and for each position in the ticket rule out all ranges
  // on that position if the number is not in the range.
  //
  // candidates maps a position in a ticket to a bitset of potential range
  // indices.
  std::vector<uint64_t> candidates(num_keys, (1ull << num_keys) - 1);
  for (const Ticket& t : valid_tickets) {
    for (uint64_t tidx{0}; tidx < num_keys; ++tidx) {
      int num = t[tidx];
      for (uint64_t ridx{0}; ridx < num_keys; ++ridx) {
        if (!std::get<1>(in.ranges[ridx]).IsValid(num)) {
          // Rule out this range at this position.
          candidates[tidx] &= ~(1ull << ridx);
        }
      }
    }
  }

  // Now, we can sort by popcount and reduce.
  std::vector<std::size_t> indices(num_keys, 0);
  std::iota(indices.begin(), indices.end(), 0);
  std::sort(indices.begin(), indices.end(),
            [&candidates](std::size_t lhs, std::size_t rhs) {
              return __builtin_popcountll(candidates[lhs]) <
                     __builtin_popcountll(candidates[rhs]);
            });
  for (auto it = indices.begin(); it != indices.end(); ++it) {
    uint64_t c = candidates[*it];
    for (auto fit = std::next(it); fit != indices.end(); ++fit) {
      candidates[*fit] &= ~c;
    }
  }
  std::unordered_map<std::string, std::size_t> result;
  for (std::size_t idx : indices) {
    std::size_t ridx = __builtin_ctzll(candidates[idx]);
    result[std::get<0>(in.ranges[ridx])] = idx;
  }
  return result;
}

uint64_t Part2(const Input& in) {
  auto mapping = DeterminePositions(in);
  uint64_t result{1};
  for (auto range : in.ranges) {
    const std::string& name = std::get<0>(range);
    if (absl::StartsWith(name, "departure")) {
      result *= in.yours[mapping[name]];
    }
  }
  return result;
}

}  // namespace day16
}  // namespace aoc20
