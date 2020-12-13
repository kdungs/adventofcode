#include "aoc20/day13.h"

#include <limits>
#include <numeric>
#include <stdexcept>
#include <string>
#include <vector>

#include "absl/strings/numbers.h"
#include "absl/strings/str_split.h"
#include "absl/strings/string_view.h"

namespace aoc20 {
namespace day13 {

std::unordered_map<int, int> ParseBusses(const std::string& line) {
  std::vector<absl::string_view> parts = absl::StrSplit(line, ',');
  std::unordered_map<int, int> busses;
  for (int idx{0}; idx < parts.size(); ++idx) {
    absl::string_view part = parts[idx];
    int bus{0};
    if (absl::SimpleAtoi(part, &bus)) {
      busses[idx] = bus;
    }
  }
  return busses;
}

int Part1(int target, const std::unordered_map<int, int>& busses) {
  auto it = busses.begin();

  int bestbus = it->second;
  int bestdiff = bestbus - target % bestbus;
  ++it;

  for (; it != busses.end(); ++it) {
    int bus = it->second;
    int diff = bus - target % bus;
    if (diff < bestdiff) {
      bestbus = bus;
      bestdiff = diff;
    }
  }
  return bestbus * bestdiff;
}

class Sequence {
 public:
  Sequence(int64_t frequency, int64_t offset)
      : frequency_{frequency}, offset_{offset} {}

  Sequence() : Sequence(1, 0) {}

  bool IsCandidate(int64_t t) const {
    return (frequency_ + t + offset_) % frequency_ == 0;
  }

  int64_t Intersection(const Sequence& other) const {
    for (int i{0}; i < std::numeric_limits<int>::max(); ++i) {
      auto t = (*this)(i);
      if (other.IsCandidate(t)) {
        return t;
      }
    }
    throw std::runtime_error("no intersection found");
  }

  Sequence operator+(const Sequence& other) const {
    int64_t t = Intersection(other);
    int64_t nf = frequency_ * other.frequency_;
    int64_t no = nf - t;
    return Sequence{nf, no};
  }

  int64_t operator()(int64_t x) const { return frequency_ * x - offset_; }

  int64_t frequency() const { return frequency_; }
  int64_t offset() const { return offset_; }

 private:
  int64_t frequency_;
  int64_t offset_;
};

int64_t Part2(const std::unordered_map<int, int>& busses) {
  std::vector<Sequence> seqs;
  seqs.reserve(busses.size());
  for (auto [offset, freq] : busses) {
    seqs.emplace_back(freq, offset);
  }

  Sequence res = std::reduce(seqs.begin(), seqs.end());
  return res(1);
}

}  // namespace day13
}  // namespace aoc20
