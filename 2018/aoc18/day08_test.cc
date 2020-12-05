#include "aoc18/day08.h"

#include "gtest/gtest.h"

using aoc18::day08::SumMetadata;
using aoc18::day08::SumValues;

const std::vector<int> kInput{2, 3, 0, 3,  10, 11, 12, 1,
                              1, 0, 1, 99, 2,  1,  1,  2};

TEST(Part1, WorksForExample) { EXPECT_EQ(138, SumMetadata(kInput)); }

TEST(Part2, WorksForExample) {
  int res = SumValues(kInput);
  EXPECT_EQ(66, res);
}
