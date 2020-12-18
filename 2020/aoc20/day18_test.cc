#include "aoc20/day18.h"

#include "gtest/gtest.h"

TEST(Part1, WorksForExamples) {
  EXPECT_EQ(71, aoc20::day18::Eval("1 + 2 * 3 + 4 * 5 + 6"));
  EXPECT_EQ(51, aoc20::day18::Eval("1 + (2 * 3) + (4 * (5 + 6))"));
  EXPECT_EQ(26, aoc20::day18::Eval("2 * 3 + (4 * 5)"));
  EXPECT_EQ(437, aoc20::day18::Eval("5 + (8 * 3 + 9 + 3 * 4 * 3)"));
  EXPECT_EQ(12240,
            aoc20::day18::Eval("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"));
  EXPECT_EQ(13632, aoc20::day18::Eval(
                       "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"));
}

TEST(Part2, WorksForExamples) {
  EXPECT_EQ(231, aoc20::day18::Eval2("1 + 2 * 3 + 4 * 5 + 6"));
  EXPECT_EQ(51, aoc20::day18::Eval2("1 + (2 * 3) + (4 * (5 + 6))"));
  EXPECT_EQ(46, aoc20::day18::Eval2("2 * 3 + (4 * 5)"));
  EXPECT_EQ(1445, aoc20::day18::Eval2("5 + (8 * 3 + 9 + 3 * 4 * 3)"));
  EXPECT_EQ(669060,
            aoc20::day18::Eval2("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"));
  EXPECT_EQ(23340, aoc20::day18::Eval2(
                       "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"));
}
