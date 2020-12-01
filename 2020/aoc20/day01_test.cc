#include "aoc20/day01.h"

#include <vector>

#include "gtest/gtest.h"

static const auto kInput = std::vector<int>{
    1721, 979, 366, 299, 675, 1456,
};

TEST(Part1, WorksForExamples) {
  EXPECT_EQ(514579, aoc20::day01::Part1(kInput));
}

TEST(Part2, WorksForExamples) {
  EXPECT_EQ(241861950, aoc20::day01::Part2(kInput));
}
