#include "aoc18/day07.h"

#include "gtest/gtest.h"

using aoc18::day07::BuildDependencyGraph;
using aoc18::day07::Graph;
using aoc18::day07::Join;
using aoc18::day07::Kahn;
using aoc18::day07::NodeTime;
using aoc18::day07::TimeKahnWithWorkers;

const std::vector<std::string> kInput = {
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin."};

const Graph kGraph = BuildDependencyGraph(kInput);

TEST(Part1, WorksForExample) {
  const std::string expected = "CABDFE";
  const std::string result = Join(Kahn(kGraph));
  EXPECT_EQ(expected, result);
}

TEST(Part2, WorksForExample) {
  const int expected = 15;
  const int result = TimeKahnWithWorkers(NodeTime, kGraph, 2);
  EXPECT_EQ(expected, result);
}
