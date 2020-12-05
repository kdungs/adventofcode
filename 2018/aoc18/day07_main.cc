#include <iostream>
#include <string>
#include <vector>

#include "aoc18/day07.h"

constexpr int kNumWorkers = 10;

int main(int argc, char* argv[]) {
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(std::cin, line)) {
    lines.push_back(line);
  }
  const auto graph = aoc18::day07::BuildDependencyGraph(lines);

  const std::string result = aoc18::day07::Join(aoc18::day07::Kahn(graph));
  std::cout << result << std::endl;

  const int result2 = aoc18::day07::TimeKahnWithWorkers(
      [](const auto& node) { return 60 + node[0] - 'A' + 1; }, graph,
      kNumWorkers);
  std::cout << result2 << std::endl;
}
