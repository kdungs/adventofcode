#include "aoc18/day16.h"

#include <array>

#include "gtest/gtest.h"

static const std::vector<std::string> kExampleLines{"Before: [3, 2, 1, 1]",
                                                    "9 2 1 2",
                                                    "After:  [3, 2, 2, 1]",
                                                    "",
                                                    "",
                                                    "",
                                                    "9 2 1 2"};

TEST(Part1, CanParseInput) {
  const auto maybe_samples_and_program =
      aoc18::day16::ParseInput(kExampleLines);
  EXPECT_TRUE(maybe_samples_and_program.has_value());
  const auto [samples, program] = maybe_samples_and_program.value();
  EXPECT_EQ(1, samples.size());

  const auto& sample = samples[0];
  aoc18::day16::Registers expected_before{3, 2, 1, 1};
  EXPECT_EQ(expected_before, sample.before);

  aoc18::day16::Instruction expected_instruction{9, 2, 1, 2};
  EXPECT_EQ(expected_instruction.code, sample.instruction.code);
  EXPECT_EQ(expected_instruction.a, sample.instruction.a);
  EXPECT_EQ(expected_instruction.b, sample.instruction.b);
  EXPECT_EQ(expected_instruction.c, sample.instruction.c);

  aoc18::day16::Registers expected_after{3, 2, 2, 1};
  EXPECT_EQ(expected_after, sample.after);

  EXPECT_EQ(1, program.size());
  EXPECT_EQ(expected_instruction.code, program[0].code);
  EXPECT_EQ(expected_instruction.a, program[0].a);
  EXPECT_EQ(expected_instruction.b, program[0].b);
  EXPECT_EQ(expected_instruction.c, program[0].c);
}

TEST(Part1, WorksForExample) {
  const auto maybe_samples_and_program =
      aoc18::day16::ParseInput(kExampleLines);
  ASSERT_TRUE(maybe_samples_and_program.has_value());
  const auto samples = std::get<0>(maybe_samples_and_program.value());
  EXPECT_EQ(1, aoc18::day16::Part1(samples));
}

TEST(Part2, WorksForExample) {
  // Cannot test this comprehensively.
}
