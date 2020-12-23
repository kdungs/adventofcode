#include "aoc20/day23.h"

#include "gtest/gtest.h"

static const std::vector<int> kExampleInput{3, 8, 9, 1, 2, 5, 4, 6, 7};

TEST(Part1, WorksForExamples) {
  EXPECT_EQ("67384529", aoc20::day23::Part1(kExampleInput));
}

TEST(Part2, WorksForExamples) {
  EXPECT_EQ(149245887792ull, aoc20::day23::Part2(kExampleInput));
}
