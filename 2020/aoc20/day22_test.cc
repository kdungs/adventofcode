#include "aoc20/day22.h"

#include <sstream>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

using aoc20::day22::Game;
using aoc20::day22::Part1;
using aoc20::day22::Part2;
using ::testing::ElementsAre;

constexpr char kExampleInput[] = R"(Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10
)";

Game ParseExampleInput() {
  std::stringstream ss{kExampleInput};
  Game g;
  ss >> g;
  return g;
}

TEST(Parsing, WorksForExamples) {
  Game g = ParseExampleInput();
  EXPECT_THAT(g.p1, ElementsAre(9, 2, 6, 3, 1));
  EXPECT_THAT(g.p2, ElementsAre(5, 8, 4, 7, 10));
}

TEST(Part1, WorksForExamples) { EXPECT_EQ(306, Part1(ParseExampleInput())); }

TEST(Part2, WorksForExamples) { EXPECT_EQ(291, Part2(ParseExampleInput())); }

TEST(Part2, Terminates) {
  constexpr char input[] = R"(Player 1:
43
19

Player 2:
2
29
14
)";
  std::stringstream ss{input};
  Game g;
  ss >> g;

  EXPECT_NE(0, Part2(g));
}
