#include "aoc20/day02.h"

#include <string>
#include <vector>

#include "gtest/gtest.h"

static const std::vector<std::string> kLines{
    "1-3 a: abcde",
    "1-3 b: cdefg",
    "2-9 c: ccccccccc",
};

TEST(Part1, WorksForExamples) {
  auto maybe_inputs = aoc20::day02::ParseInputs(kLines);
  ASSERT_TRUE(maybe_inputs.has_value());
  EXPECT_EQ(2, aoc20::day02::Part1(maybe_inputs.value()));
}

TEST(Part2, WorksForExamples) {
  auto maybe_inputs = aoc20::day02::ParseInputs(kLines);
  ASSERT_TRUE(maybe_inputs.has_value());
  EXPECT_EQ(1, aoc20::day02::Part2(maybe_inputs.value()));
}
