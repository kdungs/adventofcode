#include "aoc18/day16.h"

#include <array>

#include "gtest/gtest.h"

static const std::vector<std::string> kExampleLines{
    "Before: [3, 2, 1, 1]",
    "9 2 1 2",
    "After:  [3, 2, 2, 1]",
    "This is something else"
  };

TEST(Part1, CanParseSample) {
  const auto maybe_samples = aoc18::day16::ParseSamples(kExampleLines);
  EXPECT_TRUE(maybe_samples.has_value());
  EXPECT_EQ(1, maybe_samples.value().size());

  const aoc18::day16::Sample& sample = maybe_samples.value()[0];

  aoc18::day16::Registers expected_before{3, 2, 1, 1};
  EXPECT_EQ(expected_before, sample.before);

  aoc18::day16::Instruction expected_instruction{9, 2, 1, 2};
  EXPECT_EQ(expected_instruction.code, sample.instruction.code);
  EXPECT_EQ(expected_instruction.a, sample.instruction.a);
  EXPECT_EQ(expected_instruction.b, sample.instruction.b);
  EXPECT_EQ(expected_instruction.c, sample.instruction.c);

  aoc18::day16::Registers expected_after{3, 2, 2, 1};
  EXPECT_EQ(expected_after, sample.after);
}

TEST(Part1, WorksForExample) {
  const auto maybe_samples = aoc18::day16::ParseSamples(kExampleLines);
  ASSERT_TRUE(maybe_samples.has_value());
  EXPECT_EQ(1, aoc18::day16::Part1(maybe_samples.value()));
}

TEST(Part2, WorksForExample) {}
