#include "aoc20/day25.h"

#include "gtest/gtest.h"

constexpr int kExamplePubCard{5764801};
constexpr int kExamplePubDoor{17807724};

TEST(Part1, WorksForExamples) {
  EXPECT_EQ(14897079, aoc20::day25::Part1(kExamplePubCard, kExamplePubDoor));
}

TEST(Part2, WorksForExamples) {}
