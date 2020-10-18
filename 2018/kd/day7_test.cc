#include "kd/day7.h"

#include "gtest/gtest.h"

using kd::day7::BuildDependencyGraph;
using kd::day7::Join;
using kd::day7::Kahn;

TEST(Part1, WorksForExample) {
  const std::vector<std::string> input = {
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin."
  };
  const std::string expected = "CABDFE";

  const auto result = Join(Kahn(BuildDependencyGraph(input)));

  EXPECT_EQ(expected, result);
}
