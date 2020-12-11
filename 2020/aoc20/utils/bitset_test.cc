#include "aoc20/utils/bitset.h"

#include <vector>

#include "gtest/gtest.h"

using aoc20::utils::Bitset;

TEST(Bitset, CanInitializeEmpty) {
  for (std::size_t size : {1, 23, 64, 70, 128, 129, 1000, 10000}) {
    Bitset bs{size};
    EXPECT_EQ(0, bs.Popcount());
    EXPECT_EQ(size, bs.size());
  }
}

TEST(Bitset, CanSetBit) {
  const std::size_t size = 100;
  for (std::size_t bit{0}; bit < size; ++bit) {
    Bitset bs{size};
    bs.Set(bit);
    EXPECT_TRUE(bs.Get(bit)) << "for " << bit;
    EXPECT_EQ(1, bs.Popcount()) << " for " << bit;
  }
}

TEST(Bitset, CanSetMultipleBits) {
  const std::size_t size = 100;
  std::vector bits{1, 23, 64, 70, 71, 77};

  Bitset bs{size};
  for (auto bit : bits) {
    bs.Set(bit);
  }
  EXPECT_EQ(bits.size(), bs.Popcount());
  for (auto bit : bits) {
    EXPECT_TRUE(bs.Get(bit));
  }
}

TEST(Bitset, CanClearBit) {
  const std::size_t size = 100;
  for (std::size_t bit{0}; bit < size; ++bit) {
    Bitset bs{size};
    bs.Set(bit);
    ASSERT_TRUE(bs.Get(bit));
    bs.Clear(bit);
    EXPECT_FALSE(bs.Get(bit));
  }
}

TEST(Bitset, CanPopcount) {
  const std::size_t size = 100;
  std::vector bits{1, 23, 64, 70, 71, 77};

  Bitset bs{size};
  for (std::size_t idx{0}; idx < bits.size(); ++idx) {
    bs.Set(bits[idx]);
    ASSERT_TRUE(bs.Get(bits[idx]));
    EXPECT_EQ(idx + 1, bs.Popcount());
  }
}
