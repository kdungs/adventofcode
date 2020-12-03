#include "aoc20/day03.h"

#include "gtest/gtest.h"

static const std::vector<std::string> kInput = {
    {"..##......."}, {"#...#...#.."}, {".#....#..#."}, {"..#.#...#.#"},
    {".#...##..#."}, {"..#.##....."}, {".#.#.#....#"}, {".#........#"},
    {"#.##...#..."}, {"#...##....#"}, {".#..#...#.#"}};

TEST(Part1, WorksForExamples) {
  auto maybe_forest = aoc20::day03::ParseForest(kInput);
  ASSERT_TRUE(maybe_forest.has_value());

  EXPECT_EQ(7, aoc20::day03::Part1(maybe_forest.value()));
}

TEST(Part2, WorksForExamples) {
  auto maybe_forest = aoc20::day03::ParseForest(kInput);
  ASSERT_TRUE(maybe_forest.has_value());

  EXPECT_EQ(336, aoc20::day03::Part2(maybe_forest.value()));
}
