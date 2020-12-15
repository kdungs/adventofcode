#include "aoc20/day15.h"

#include "gtest/gtest.h"

TEST(Part1, WorksForExamples) {
  EXPECT_EQ(436, aoc20::day15::Part1(std::vector<int>{0, 3, 6}));
  EXPECT_EQ(1, aoc20::day15::Part1(std::vector<int>{1, 3, 2}));
  EXPECT_EQ(10, aoc20::day15::Part1(std::vector<int>{2, 1, 3}));
  EXPECT_EQ(27, aoc20::day15::Part1(std::vector<int>{1, 2, 3}));
  EXPECT_EQ(78, aoc20::day15::Part1(std::vector<int>{2, 3, 1}));
  EXPECT_EQ(438, aoc20::day15::Part1(std::vector<int>{3, 2, 1}));
  EXPECT_EQ(1836, aoc20::day15::Part1(std::vector<int>{3, 1, 2}));
}

TEST(Part2, WorksForExamples) {
  EXPECT_EQ(175594, aoc20::day15::Part2(std::vector<int>{0, 3, 6}));
  EXPECT_EQ(2578, aoc20::day15::Part2(std::vector<int>{1, 3, 2}));
  EXPECT_EQ(3544142, aoc20::day15::Part2(std::vector<int>{2, 1, 3}));
  EXPECT_EQ(261214, aoc20::day15::Part2(std::vector<int>{1, 2, 3}));
  EXPECT_EQ(6895259, aoc20::day15::Part2(std::vector<int>{2, 3, 1}));
  EXPECT_EQ(18, aoc20::day15::Part2(std::vector<int>{3, 2, 1}));
  EXPECT_EQ(362, aoc20::day15::Part2(std::vector<int>{3, 1, 2}));
}
