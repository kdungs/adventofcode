#include "aoc20/day09.h"

#include <vector>

#include "gtest/gtest.h"

static const std::vector<int> kExample{35,  20,  15,  25,  47,  40,  62,
                                       55,  65,  95,  102, 117, 150, 182,
                                       127, 219, 299, 277, 309, 576};

TEST(Part1, WorksForExamples) {
  EXPECT_EQ(127, aoc20::day09::Part1(kExample, 5));
}

TEST(Part2, WorksForExamples) {
  EXPECT_EQ(62, aoc20::day09::Part2(kExample, 5));
}
