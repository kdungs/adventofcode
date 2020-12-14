#include "aoc20/day14.h"

#include <string>
#include <vector>

#include "gtest/gtest.h"

TEST(Part1, WorksForExamples) {
  std::vector<std::string> lines{
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0",
  };
  EXPECT_EQ(165, aoc20::day14::Part1(lines));
}

TEST(Part2, WorksForExamples) {
  std::vector<std::string> lines{
      "mask = 000000000000000000000000000000X1001X", "mem[42] = 100",
      "mask = 00000000000000000000000000000000X0XX", "mem[26] = 1"};
  EXPECT_EQ(208, aoc20::day14::Part2(lines));
}
