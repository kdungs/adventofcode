#include "aoc20/day06.h"

#include <sstream>

#include "gtest/gtest.h"

constexpr char kExampleInput[] = R"(abc

a
b
c

ab
ac

a
a
a
a

b
)";

TEST(Part1, WorksForExamples) {
  std::stringstream ss{kExampleInput};
  auto blocks = aoc20::day06::ParseInput(ss);
  EXPECT_EQ(11, aoc20::day06::Part1(blocks));
}

TEST(Part2, WorksForExamples) {
  std::stringstream ss{kExampleInput};
  auto blocks = aoc20::day06::ParseInput(ss);
  EXPECT_EQ(6, aoc20::day06::Part2(blocks));
}
