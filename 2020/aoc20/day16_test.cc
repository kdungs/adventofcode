#include "aoc20/day16.h"

#include "gtest/gtest.h"

constexpr char kExampleInput[] = R"(class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
)";

aoc20::day16::Input ParsedExampleInput(const char* in) {
  auto maybe_input = aoc20::day16::ParseInput(in);
  return maybe_input.value();
}

TEST(ParseInput, WorksForExample) {
  auto maybe_input = aoc20::day16::ParseInput(kExampleInput);
  ASSERT_TRUE(maybe_input.has_value());
  auto input = maybe_input.value();
  EXPECT_EQ(3, input.ranges.size());
  EXPECT_EQ(3, input.yours.size());
  EXPECT_EQ(4, input.others.size());
}

TEST(Part1, WorksForExamples) {
  EXPECT_EQ(71, aoc20::day16::Part1(ParsedExampleInput(kExampleInput)));
}

TEST(Part2, WorksForExamples) {
  auto input = ParsedExampleInput(R"(class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
)");
  auto mapping = aoc20::day16::DeterminePositions(input);

  EXPECT_EQ(12, input.yours[mapping["class"]]);
  EXPECT_EQ(11, input.yours[mapping["row"]]);
  EXPECT_EQ(13, input.yours[mapping["seat"]]);
}
