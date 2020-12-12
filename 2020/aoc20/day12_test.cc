#include "aoc20/day12.h"

#include "gtest/gtest.h"

constexpr char kExampleInput[] = R"(F10
N3
F7
R90
F11
)";

TEST(Part1, WorksForExamples) {
  std::stringstream ss{kExampleInput};
  EXPECT_EQ(25, aoc20::day12::Part1(ss));
}

TEST(Part2, WorksForExamples) {
  std::stringstream ss{kExampleInput};
  EXPECT_EQ(286, aoc20::day12::Part2(ss));
}
