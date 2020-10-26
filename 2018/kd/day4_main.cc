#include <iostream>
#include <string>
#include <vector>

#include "kd/day4.h"

int main(int argc, char *argv[]) {
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(std::cin, line)) {
    lines.push_back(line);
  }

  const auto maybe_shift_plan = kd::day4::ParseShiftPlan(lines);
  if (!maybe_shift_plan.has_value()) {
    std::cerr << "unable to parse input" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << kd::day4::SolvePart1(maybe_shift_plan.value()) << std::endl;
  std::cout << kd::day4::SolvePart2(maybe_shift_plan.value()) << std::endl;
}
