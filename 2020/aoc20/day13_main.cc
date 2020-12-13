#include <iostream>
#include <unordered_map>

#include "aoc20/day13.h"

int main(int argc, char *argv[]) {
  int target;
  std::cin >> target;

  std::string line;
  std::getline(std::cin, line);  // read rest of previous line
  std::getline(std::cin, line);
  std::unordered_map<int, int> busses = aoc20::day13::ParseBusses(line);

  std::cout << "Part 1: " << aoc20::day13::Part1(target, busses) << std::endl;
  std::cout << "Part 2: " << aoc20::day13::Part2(busses) << std::endl;
}
