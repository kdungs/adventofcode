#include "kd/day7.h"

#include <iostream>
#include <string>
#include <vector>


int main(int argc, char *argv[]) {
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(std::cin, line)) {
    lines.push_back(line);
  }

  const std::string result = kd::day7::Join(
    kd::day7::Kahn(
      kd::day7::BuildDependencyGraph(lines)
    )
  );
  std::cout << result << '\n';
}
