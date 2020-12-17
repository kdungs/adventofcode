#include "aoc20/day17.h"

#include <string>
#include <vector>

#include "gtest/gtest.h"

static const std::vector<std::string> kExampleInput{".#.", "..#", "###"};

TEST(Part1, WorksForExamples) {
  EXPECT_EQ(112, aoc20::day17::Part1(kExampleInput));
}

TEST(Part2, WorksForExamples) {
  EXPECT_EQ(848, aoc20::day17::Part2(kExampleInput));
}
