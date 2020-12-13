#include "aoc20/day13.h"

#include <unordered_map>

#include "gtest/gtest.h"

static const std::unordered_map<int, int> kExampleBusses{
    {0, 7}, {1, 13}, {4, 59}, {6, 31}, {7, 19}};

TEST(Part1, WorksForExamples) {
  int target = 939;
  EXPECT_EQ(295, aoc20::day13::Part1(target, kExampleBusses));
}

TEST(Part2, WorksForExamples) {
  EXPECT_EQ(1068781, aoc20::day13::Part2(kExampleBusses));

  EXPECT_EQ(3417, aoc20::day13::Part2(
                      std::unordered_map<int, int>{{0, 17}, {2, 13}, {3, 19}}));
  EXPECT_EQ(754018, aoc20::day13::Part2(std::unordered_map<int, int>{
                        {0, 67}, {1, 7}, {2, 59}, {3, 61}}));
  EXPECT_EQ(754018, aoc20::day13::Part2(std::unordered_map<int, int>{
                        {0, 67}, {1, 7}, {2, 59}, {3, 61}}));
  EXPECT_EQ(779210, aoc20::day13::Part2(std::unordered_map<int, int>{
                        {0, 67}, {2, 7}, {3, 59}, {4, 61}}));
  EXPECT_EQ(1261476, aoc20::day13::Part2(std::unordered_map<int, int>{
                         {0, 67}, {1, 7}, {3, 59}, {4, 61}}));
  EXPECT_EQ(1202161486, aoc20::day13::Part2(std::unordered_map<int, int>{
                            {0, 1789}, {1, 37}, {2, 47}, {3, 1889}}));
}
