#include <iostream>
#include <string>
#include <vector>

#include "kd/day7.h"

constexpr int kNumWorkers = 10;

int main(int argc, char* argv[]) {
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(std::cin, line)) {
    lines.push_back(line);
  }
  const auto graph = kd::day7::BuildDependencyGraph(lines);

  const std::string result = kd::day7::Join(kd::day7::Kahn(graph));
  std::cout << result << std::endl;

  const int result2 = kd::day7::TimeKahnWithWorkers(
      [](const auto& node) { return 60 + node[0] - 'A' + 1; }, graph,
      kNumWorkers);
  std::cout << result2 << std::endl;
}
