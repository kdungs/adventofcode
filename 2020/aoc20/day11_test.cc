#include "aoc20/day11.h"

#include <sstream>

#include "gtest/gtest.h"

using aoc20::day11::ParseSeatPlan;
using aoc20::day11::Part1;
using aoc20::day11::Part2;

static const std::vector<std::string> kExampleInput{
    "L.LL.LL.LL", "LLLLLLL.LL", "L.L.L..L..", "LLLL.LL.LL", "L.LL.LL.LL",
    "L.LLLLL.LL", "..L.L.....", "LLLLLLLLLL", "L.LLLLLL.L", "L.LLLLL.LL"};

TEST(ParseSeatPlan, WorksForExample) {
  auto maybe_plan = ParseSeatPlan(kExampleInput);
  ASSERT_TRUE(maybe_plan.has_value());
  auto plan = maybe_plan.value();
  EXPECT_EQ(71, plan.size());
}

TEST(Part1, WorksForExamples) {
  auto maybe_plan = ParseSeatPlan(kExampleInput);
  ASSERT_TRUE(maybe_plan.has_value());
  auto plan = maybe_plan.value();
  EXPECT_EQ(37, Part1(plan));
}

TEST(Part2, WorksForExamples) {
  auto maybe_plan = ParseSeatPlan(kExampleInput);
  ASSERT_TRUE(maybe_plan.has_value());
  auto plan = maybe_plan.value();
  EXPECT_EQ(26, Part2(plan));
}
