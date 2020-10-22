#include "kd/day7.h"

#include "gtest/gtest.h"

using kd::day7::BuildDependencyGraph;
using kd::day7::Graph;
using kd::day7::Join;
using kd::day7::Kahn;
using kd::day7::NodeTime;
using kd::day7::TimeKahnWithWorkers;

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
