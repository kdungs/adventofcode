#include "aoc20/day19.h"

#include <algorithm>
#include <memory>
#include <set>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "absl/strings/match.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_split.h"
#include "absl/strings/string_view.h"

namespace aoc20 {
namespace day19 {

class PosOrFail {
 public:
  bool IsFail() const { return value_ < 0; }

  std::size_t Pos() const {
    // Don't need error checking for now.
    return value_;
  }

  static PosOrFail Pos(std::size_t pos) {
    return PosOrFail{static_cast<int64_t>(pos)};
  }

  static PosOrFail Fail() { return PosOrFail{-1}; }

 private:
  explicit PosOrFail(int64_t value) : value_{value} {}
  int64_t value_;
};

class Rule;
using Ruleset = std::unordered_map<absl::string_view, std::unique_ptr<Rule>>;

const Rule* GetRule(const Ruleset& rs, absl::string_view key) {
  auto it = rs.find(key);
  if (it == rs.end()) {
    throw std::runtime_error(absl::StrCat("unknown rule: ", key));
  }
  return it->second.get();
}

class Rule {
 public:
  virtual ~Rule() = default;
  virtual PosOrFail Eval(const Ruleset& rs, absl::string_view str,
                         std::size_t pos) const = 0;
  virtual std::set<std::string> Enumerate(const Ruleset& rs) const = 0;
};

class CharRule : public Rule {
 public:
  explicit CharRule(char c) : c_{c} {}

  PosOrFail Eval(const Ruleset& rs, absl::string_view str,
                 std::size_t pos) const final {
    if (str[pos] != c_) {
      return PosOrFail::Fail();
    }
    return PosOrFail::Pos(pos + 1);
  }

  std::set<std::string> Enumerate(const Ruleset& rs) const final {
    return {std::string(1, c_)};
  }

 private:
  char c_;
};

class SubRules : public Rule {
 public:
  explicit SubRules(std::vector<absl::string_view> subs) : subs_{subs} {}

  PosOrFail Eval(const Ruleset& rs, absl::string_view str,
                 std::size_t pos) const final {
    for (absl::string_view sub : subs_) {
      PosOrFail pof = GetRule(rs, sub)->Eval(rs, str, pos);
      if (pof.IsFail()) {
        return pof;
      }
      pos = pof.Pos();
    }
    return PosOrFail::Pos(pos);
  }

  std::set<std::string> Enumerate(const Ruleset& rs) const final {
    std::set<std::string> result{""};
    for (absl::string_view sub : subs_) {
      std::set<std::string> merged{};
      for (const std::string& a : GetRule(rs, sub)->Enumerate(rs)) {
        for (const std::string& r : result) {
          merged.insert(r + a);
        }
      }
      result = merged;
    }
    return result;
  }

 private:
  std::vector<absl::string_view> subs_;
};

class OrRule : public Rule {
 public:
  OrRule(std::unique_ptr<Rule>&& lhs, std::unique_ptr<Rule>&& rhs)
      : lhs_{std::move(lhs)}, rhs_{std::move(rhs)} {}

  PosOrFail Eval(const Ruleset& rs, absl::string_view str,
                 std::size_t pos) const final {
    PosOrFail pof = lhs_->Eval(rs, str, pos);
    if (!pof.IsFail()) {
      return pof;
    }
    return rhs_->Eval(rs, str, pos);
  }

  std::set<std::string> Enumerate(const Ruleset& rs) const final {
    std::set<std::string> l = lhs_->Enumerate(rs);
    std::set<std::string> r = rhs_->Enumerate(rs);
    l.insert(r.begin(), r.end());
    return l;
  }

 private:
  std::unique_ptr<Rule> lhs_;
  std::unique_ptr<Rule> rhs_;
};

std::unique_ptr<Rule> ParseRule(absl::string_view rulestr) {
  if (rulestr[0] == '"') {
    return std::make_unique<CharRule>(rulestr[1]);
  }
  std::vector<absl::string_view> orparts = absl::StrSplit(rulestr, " | ");
  if (orparts.size() == 1) {
    return std::make_unique<SubRules>(absl::StrSplit(orparts[0], ' '));
  } else if (orparts.size() == 2) {
    return std::make_unique<OrRule>(ParseRule(orparts[0]),
                                    ParseRule(orparts[1]));
  }
  throw std::runtime_error(absl::StrCat("unable to parse rule: ", rulestr));
}

template <typename ForwardIt>
Ruleset ParseRuleset(ForwardIt begin, ForwardIt end) {
  Ruleset rs;
  for (; begin != end; ++begin) {
    const std::string& line = *begin;
    std::vector<absl::string_view> parts = absl::StrSplit(line, ": ");
    rs[parts[0]] = ParseRule(parts[1]);
  }
  return rs;
}

bool MatchesRule(const Ruleset& rs, const Rule* rule, absl::string_view str) {
  PosOrFail pof = rule->Eval(rs, str, 0);
  if (pof.IsFail()) {
    return false;
  }
  return pof.Pos() == str.size();
}

int Part1(const std::vector<std::string>& lines) {
  auto split = std::find(lines.begin(), lines.end(), "");
  auto rs = ParseRuleset(lines.begin(), split);
  const Rule* r0 = GetRule(rs, "0");

  int count{0};
  ++split;
  for (; split != lines.end(); ++split) {
    if (MatchesRule(rs, r0, *split)) {
      ++count;
    }
  }
  return count;
}

int Part2(const std::vector<std::string>& lines) {
  auto split = std::find(lines.begin(), lines.end(), "");
  auto rs = ParseRuleset(lines.begin(), split);

  // We know that r0 is 8 11 because that's what it always is.
  // We also know that 8: 42 | 42 8 which is equivalent to 42+
  //              and 11: 42 31 | 42 11 31 which is 42 (42...) (31...) 31
  auto e42 = rs["42"]->Enumerate(rs);
  auto e31 = rs["31"]->Enumerate(rs);

  // Generates all possible versions of str with any combination of 42s removed
  // from the front from at least one up to as many as possible.
  const auto G8 = [&e42](absl::string_view str) -> std::set<absl::string_view> {
    std::set<absl::string_view> all{};
    std::set<absl::string_view> lst{str};
    while (!lst.empty()) {
      std::set<absl::string_view> nxt;
      for (const std::string& option : e42) {
        for (absl::string_view s : lst) {
          if (absl::StartsWith(s, option)) {
            s.remove_prefix(option.size());
            nxt.insert(s);
          }
        }
      }
      all.insert(nxt.begin(), nxt.end());
      lst = nxt;
    }
    return all;
  };

  // Checks whether a given string view can be built from an equal number of
  // 42s and 31s. What a nightmare.
  const auto M11 = [&e42, &e31](absl::string_view str) -> bool {
    if (str.size() == 0) {
      return false;
    }
    std::set<absl::string_view> cnd{str};
    while (!cnd.empty()) {
      std::set<absl::string_view> nxt;
      for (absl::string_view p42 : e42) {
        for (absl::string_view s31 : e31) {
          for (absl::string_view c : cnd) {
            if (absl::StartsWith(c, p42)) {
              c.remove_prefix(p42.size());
              if (absl::EndsWith(c, s31)) {
                c.remove_suffix(s31.size());
                if (c.size() == 0) {
                  return true;
                }
                nxt.insert(c);
              }
            }
          }
        }
      }
      cnd = nxt;
    }
    return false;
  };

  int count{0};
  ++split;
  for (; split != lines.end(); ++split) {
    for (absl::string_view g : G8(*split)) {
      if (M11(g)) {
        ++count;
        break;
      }
    }
  }
  return count;
}

}  // namespace day19
}  // namespace aoc20
