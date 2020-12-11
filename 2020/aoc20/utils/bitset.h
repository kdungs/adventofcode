#ifndef AOC20_UTILS_BITSET_H_
#define AOC20_UTILS_BITSET_H_

#include <numeric>
#include <cstdint>
#include <vector>

// Ideas
//   - Small size optimisations: if our data would fit into the size of a
//     vector container we can just use that space on the stack directly.
//   - Bitwise operations between bitsets of the same size
//   - Bitwise operations between bitsets of different sizes
//   - Better error handling
// Decide on whether we want to automatically allow larger size or really stick
// to what the user gave us. The way it is now, we'd have to be careful to
// implement something like Flip because if we set bits above size the result
// of Popcount would be affected.

namespace aoc20 {
namespace utils {

namespace {
  // The compiler probably does those for us, anyway...

  inline uint32_t div32(uint64_t x) {
    return x >> 5;
  }

  inline uint32_t mod32(uint64_t x) {
    return x & 31;
  }
}  // namespace

class Bitset {
 public:
  explicit Bitset(std::size_t size)
    : size_(size),
      data_(div32(size) + 1, 0) {}

  void Set(std::size_t idx) {
    data_[bucket(idx)] |= bidxmask(idx);
  }

  void Clear(std::size_t idx) {
    data_[bucket(idx)] &= ~bidxmask(idx);
  }

  bool Get(std::size_t idx) const {
    return data_[bucket(idx)] & bidxmask(idx);
  }

  std::size_t Popcount() const {
    return std::accumulate(
      data_.begin(),
      data_.end(),
      0ull,
      [](uint64_t acc, uint64_t d) {
        return acc + __builtin_popcountll(d);
      }
    );
  }

  std::size_t size() const { return size_; }

  bool operator==(const Bitset& other) const {
    return (size() == other.size()) && (data_ == other.data_);
  }

 private:
  std::size_t bucket(std::size_t idx) const {
    return div32(idx);
  }

  uint32_t bidxmask(std::size_t idx) const {
    return 1 << mod32(idx);
  }

  std::size_t size_;
  std::vector<uint32_t> data_;
};

}  // namespace utils
}  // namespace aoc20

#endif  // AOC20_UTILS_BITSET_H_
