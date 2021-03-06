#include "aoc18/day09.h"

#include <string>
#include <unordered_map>

#include "gtest/gtest.h"

using aoc18::day09::HighScore;
using aoc18::day09::ParseInput;

TEST(Part1, WorksForExamples) {
  const std::unordered_map<std::string, int> cases{
      {"10 players; last marble is worth 1618 points", 8317},
      {"13 players; last marble is worth 7999 points", 146373},
      {"17 players; last marble is worth 1104 points", 2764},
      {"21 players; last marble is worth 6111 points", 54718},
      {"30 players; last marble is worth 5807 points", 37305}};

  for (const auto& kv : cases) {
    auto maybe_input = ParseInput(kv.first);
    ASSERT_TRUE(maybe_input.has_value()) << "invalid input";
    const int expected = kv.second;
    const int result = HighScore(maybe_input.value());
    EXPECT_EQ(expected, result) << "incorrect result";
  }
}
