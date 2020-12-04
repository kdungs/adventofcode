#include "aoc20/day04.h"

#include <algorithm>
#include <initializer_list>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

#include "absl/strings/numbers.h"
#include "absl/strings/str_split.h"
#include "absl/strings/string_view.h"

namespace aoc20 {
namespace day04 {

namespace {
static const std::vector<std::string> kRequiredFields{
    "byr",  // (Birth Year)
    "iyr",  // (Issue Year)
    "eyr",  // (Expiration Year)
    "hgt",  // (Height)
    "hcl",  // (Hair Color)
    "ecl",  // (Eye Color)
    "pid"   // (Passport ID)
};
}  // namespace

Passport ParsePassport(absl::string_view block) {
  Passport passport;
  for (const auto& entry : absl::StrSplit(block, absl::ByAnyChar(" \n"))) {
    const std::vector<std::string> kv = absl::StrSplit(entry, ':');
    if (kv.size() < 2) {
      // Can happen e.g for the last passport that still has a newline
      continue;
    }
    passport[kv[0]] = kv[1];
  }
  return passport;
}

std::vector<Passport> ParsePassports(const std::string& input) {
  std::vector<Passport> passports;
  for (absl::string_view block : absl::StrSplit(input, "\n\n")) {
    passports.push_back(ParsePassport(block));
  }
  return passports;
}

bool HasRequiredFields(const Passport& passport) {
  for (const auto& field : kRequiredFields) {
    if (passport.count(field) == 0) {
      return false;
    }
  }
  return true;
}

int Part1(const std::vector<Passport>& passports) {
  return std::count_if(passports.begin(), passports.end(), HasRequiredFields);
}

class Validator {
 public:
  virtual ~Validator() = default;
  virtual bool Is(absl::string_view s) = 0;
};

struct NumberInRange : Validator {
  NumberInRange(int min, int max) : min{min}, max{max} {}

  bool Is(absl::string_view s) override {
    int x;
    if (!absl::SimpleAtoi(s, &x)) {
      // Not a number.
      return false;
    }
    return (min <= x) && (x <= max);
  }

  static std::unique_ptr<Validator> create(int min, int max) {
    return std::unique_ptr<Validator>(new NumberInRange{min, max});
  }

  int min;
  int max;
};

struct HeightInRange : Validator {
  HeightInRange(int mincm, int maxcm, int minin, int maxin)
      : mincm{mincm}, maxcm{maxcm}, minin{minin}, maxin{maxin} {}

  bool Is(absl::string_view s) override {
    int min = mincm;
    int max = maxcm;
    if (s[s.size() - 1] == 'n') {
      // inches not centimeter
      min = minin;
      max = maxin;
    }
    s.remove_suffix(2);
    int height;
    if (!absl::SimpleAtoi(s, &height)) {
      // Not a number
      return false;
    }
    return (min <= height) && (height <= max);
  }

  static std::unique_ptr<Validator> create(int mincm, int maxcm, int minin,
                                           int maxin) {
    return std::unique_ptr<Validator>(
        new HeightInRange{mincm, maxcm, minin, maxin});
  }

  int mincm;
  int maxcm;
  int minin;
  int maxin;
};

struct Color : Validator {
  bool Is(absl::string_view s) override {
    if (s[0] != '#') {
      return false;
    }
    if (s.size() != 7) {
      return false;
    }
    s.remove_prefix(1);
    int _;
    if (!absl::numbers_internal::safe_strtoi_base(s, &_, 16)) {
      return false;
    }
    return true;
  }

  static std::unique_ptr<Validator> create() {
    return std::unique_ptr<Validator>(new Color);
  }
};

struct OneOf : Validator {
  explicit OneOf(std::initializer_list<std::string> args) : values{args} {}

  bool Is(absl::string_view s) override {
    return values.count(std::string(s)) != 0;
  }

  template <typename... Args>
  static std::unique_ptr<Validator> create(Args... args) {
    return std::unique_ptr<Validator>(new OneOf{{args...}});
  }

  std::unordered_set<std::string> values;
};

struct NumberOfDigits : Validator {
  explicit NumberOfDigits(int n) : n{n} {}

  bool Is(absl::string_view s) override {
    if (s.size() != n) {
      return false;
    }
    int _;
    return absl::SimpleAtoi(s, &_);
  }

  static std::unique_ptr<Validator> create(int n) {
    return std::unique_ptr<Validator>(new NumberOfDigits{n});
  }

  int n;
};

bool HasValidFields(const Passport& passport) {
  std::unordered_map<std::string, std::unique_ptr<Validator>> validators;
  validators["byr"] = NumberInRange::create(1920, 2002);
  validators["iyr"] = NumberInRange::create(2010, 2020);
  validators["eyr"] = NumberInRange::create(2020, 2030);
  validators["hgt"] = HeightInRange::create(150, 193, 59, 76);
  validators["hcl"] = Color::create();
  validators["ecl"] =
      OneOf::create("amb", "blu", "brn", "gry", "grn", "hzl", "oth");
  validators["pid"] = NumberOfDigits::create(9);

  for (const auto& [field, validator] : validators) {
    if (!validator->Is(passport.at(field))) {
      return false;
    }
  }
  return true;
}

int Part2(const std::vector<Passport>& passports) {
  return std::count_if(passports.begin(), passports.end(),
                       [](const Passport& p) {
                         return HasRequiredFields(p) && HasValidFields(p);
                       });
}

}  // namespace day04
}  // namespace aoc20
