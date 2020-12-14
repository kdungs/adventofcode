#include "aoc20/day14.h"

#include <cstdint>
#include <iostream>
#include <numeric>
#include <regex>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace aoc20 {
namespace day14 {

template <typename M>
uint64_t RunWithMemory(const std::vector<std::string>& lines) {
  static const std::regex re{R"(^mem\[(\d+)\] = (\d+)$)"};
  M mem;
  for (const auto& line : lines) {
    std::string_view sv(line);
    if (sv[1] == 'a') {
      // if (sv.starts_with("mask")) {  // C++20
      sv.remove_prefix(7);
      mem.SetMask(sv);
    } else {
      std::smatch m;
      if (!std::regex_match(line, m, re)) {
        std::cerr << "unable to parse line: " << sv << std::endl;
        return -1;
      }
      uint64_t addr = std::stoull(m[1]);
      uint64_t value = std::stoull(m[2]);
      mem.Set(addr, value);
    }
  }
  return std::accumulate(
      mem.data().begin(), mem.data().end(), 0ull,
      [](uint64_t acc, const auto& kv) { return acc + kv.second; });
}

class Memory {
 public:
  void SetMask(std::string_view str) {
    if (str.size() != 36) {
      return;
    }
    mask_one_ = 0;
    mask_zero_ = 0;
    for (std::size_t idx{0}; idx < 36; ++idx) {
      char c = str[str.size() - 1 - idx];
      if (c == '1') {
        mask_one_ |= (1ull << idx);
      } else if (c == '0') {
        mask_zero_ |= (1ull << idx);
      }
    }
  }

  void Set(int addr, uint64_t value) {
    value |= mask_one_;
    value &= ~mask_zero_;
    data_[addr] = value;
  }

  const std::unordered_map<uint64_t, uint64_t>& data() const { return data_; }

 private:
  uint64_t mask_one_;
  uint64_t mask_zero_;
  std::unordered_map<uint64_t, uint64_t> data_;
};

uint64_t Part1(const std::vector<std::string>& lines) {
  return RunWithMemory<Memory>(lines);
}

class FloatingMemory {
 public:
  void SetMask(std::string_view str) {
    if (str.size() != 36) {
      return;
    }
    mask_one_ = 0;
    floating_pos_.clear();
    for (std::size_t idx{0}; idx < 36; ++idx) {
      char c = str[str.size() - 1 - idx];
      if (c == '1') {
        mask_one_ |= (1ull << idx);
      } else if (c == 'X') {
        floating_pos_.push_back(idx);
      }
    }
  }

  void Set(uint64_t addr, uint64_t value) {
    addr |= mask_one_;

    std::vector<uint64_t> addrs{addr};
    for (uint64_t fpos : floating_pos_) {
      const std::size_t n = addrs.size();
      for (std::size_t idx{0}; idx < n; ++idx) {
        addrs.push_back(addrs[idx] ^ (1ull << fpos));
      }
    }

    for (uint64_t a : addrs) {
      data_[a] = value;
    }
  }

  const std::unordered_map<uint64_t, uint64_t>& data() const { return data_; }

 private:
  uint64_t mask_one_;
  std::vector<uint64_t> floating_pos_;
  std::unordered_map<uint64_t, uint64_t> data_;
};

uint64_t Part2(const std::vector<std::string>& lines) {
  return RunWithMemory<FloatingMemory>(lines);
}

}  // namespace day14
}  // namespace aoc20
