#include "aoc20/day05.h"

#include "gtest/gtest.h"

TEST(Part1, WorksForExamples) {
  EXPECT_EQ(357, "FBFBBFFRLR");
  EXPECT_EQ(567, "BFFFBBFRRR");
  EXPECT_EQ(119, "FFFBBBFRRR");
  EXPECT_EQ(820, "BBFFBBFRLL");
}

TEST(Part2, WorksForExamples) {
  // We can't really unit test Part2 all that well without having a full input.
  // Since this is not given in the examples, and unit tests here only focus on
  // the examples, we're allowed to skip unit testing Part2, today. :)
}
