#include <iostream>
#include <string>
#include <vector>

#include "aoc20/day11.h"

int main(int argc, char *argv[]) {
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(std::cin, line)) {
    lines.push_back(line);
  }

  auto maybe_seat_plan = aoc20::day11::ParseSeatPlan(lines);
  if (!maybe_seat_plan.has_value()) {
    std::cerr << "unable to parse seat plan\n";
    return 1;
  }
  auto seat_plan = maybe_seat_plan.value();

  std::cout << "Part 1: " << aoc20::day11::Part1(seat_plan) << std::endl;
  std::cout << "Part 2: " << aoc20::day11::Part2(seat_plan) << std::endl;
}
