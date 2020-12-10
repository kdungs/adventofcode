#include "aoc20/day10.h"

#include <vector>

#include "gtest/gtest.h"

static const std::vector<int> kExample1{16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4};

static const std::vector<int> kExample2{
    28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38,
    39, 11, 1,  32, 25, 35, 8,  17, 7,  9,  4,  2,  34, 10, 3};

TEST(Part1, WorksForExamples) {
  EXPECT_EQ(35, aoc20::day10::Part1(kExample1));
  EXPECT_EQ(220, aoc20::day10::Part1(kExample2));
}

TEST(Part2, WorksForExamples) {
  EXPECT_EQ(8, aoc20::day10::Part2(kExample1));
  EXPECT_EQ(19208, aoc20::day10::Part2(kExample2));
}
