#include "aoc20/day07.h"

#include <iostream>
#include <optional>
#include <regex>
#include <utility>
#include <vector>

#include "absl/strings/numbers.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_split.h"
#include "absl/strings/string_view.h"

namespace aoc20 {
namespace day07 {

std::optional<std::pair<Color, Count>> ParseRule(const std::string& line) {
  std::vector<absl::string_view> tokens = absl::StrSplit(line, ' ');
  if (tokens.size() < 7) {
    return std::nullopt;
  }

  Color c{absl::StrCat(tokens[0], " ", tokens[1])};
  Count cs;
  bool next{true};
  int idx{4};
  while (next) {
    if (tokens[idx] == "no") {
      next = false;
      continue;
    }
    if (tokens.size() < idx + 4) {
      return std::nullopt;
    }
    Color cc{absl::StrCat(tokens[idx + 1], " ", tokens[idx + 2])};
    std::size_t n;
    if (!absl::SimpleAtoi(tokens[idx], &n)) {
      return std::nullopt;
    }
    cs[cc] = n;
    auto tok = tokens[idx + 3];
    if (tok[tok.size() - 1] != ',') {
      next = false;
    }
    idx += 4;
  }
  return std::make_pair(c, cs);
}

std::optional<Rules> ParseRules(std::istream& in) {
  Rules rules;
  std::string line;
  while (std::getline(in, line)) {
    auto maybe_color_and_count = ParseRule(line);
    if (!maybe_color_and_count.has_value()) {
      return std::nullopt;
    }
    rules.insert(maybe_color_and_count.value());
  }
  return rules;
}

class CanContainColor {
 public:
  CanContainColor(const Rules& r, Color c) : rules_{&r}, c_{std::move(c)} {}

  bool Can(Color oc) {
    auto it = can_contain_.find(oc);
    if (it != can_contain_.end()) {
      return it->second;
    }
    const auto& cs = rules_->at(oc);
    bool can{false};
    for (const auto& [c, _] : cs) {
      if (c == c_ || Can(c)) {
        can = true;
        break;
      }
    }
    can_contain_[oc] = can;
    return can;
  }

  int Count() {
    return std::count_if(rules_->begin(), rules_->end(),
                         [this](const auto& kv) { return Can(kv.first); });
  }

 private:
  const Rules* const rules_;
  Color c_;
  std::unordered_map<Color, bool> can_contain_;
};

int Part1(const Rules& rules) {
  CanContainColor csg{rules, "shiny gold"};
  return csg.Count();
}

class ContainedBagsCounter {
 public:
  explicit ContainedBagsCounter(const Rules& r) : rules_{&r} {}

  std::size_t Count(const Color& c) {
    auto it = counts_.find(c);
    if (it != counts_.end()) {
      return it->second;
    }
    const auto& cs = rules_->at(c);
    std::size_t cnt{0};
    for (const auto& [oc, n] : cs) {
      cnt += n * (1 + Count(oc));
    }
    counts_[c] = cnt;
    return cnt;
  }

 private:
  const Rules* const rules_;
  std::unordered_map<Color, std::size_t> counts_;
};

int Part2(const Rules& rules) {
  ContainedBagsCounter cbc(rules);
  return cbc.Count("shiny gold");
}

}  // namespace day07
}  // namespace aoc20
