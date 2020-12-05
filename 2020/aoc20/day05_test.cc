#include "aoc20/day05.h"

#include "gtest/gtest.h"

TEST(Part1, WorksForExamples) {
  EXPECT_EQ(357, aoc20::day05::GetSeatId("FBFBBFFRLR"));
  EXPECT_EQ(567, aoc20::day05::GetSeatId("BFFFBBFRRR"));
  EXPECT_EQ(119, aoc20::day05::GetSeatId("FFFBBBFRRR"));
  EXPECT_EQ(820, aoc20::day05::GetSeatId("BBFFBBFRLL"));
}

TEST(Part2, WorksForExamples) {
  // We can't really unit test Part2 all that well without having a full input.
  // Since this is not given in the examples, and unit tests here only focus on
  // the examples, we're allowed to skip unit testing Part2, today. :)
  //
  // Instead we'll run some benchmarks on different implementations.
}
