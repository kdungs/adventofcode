#include <iostream>
#include <sstream>

#include "aoc20/day12.h"

int main(int argc, char *argv[]) {
  std::istreambuf_iterator<char> begin(std::cin), end;
  std::string input(begin, end);

  std::stringstream ss1{input};
  std::cout << "Part 1: " << aoc20::day12::Part1(ss1) << std::endl;
  std::stringstream ss2{input};
  std::cout << "Part 2: " << aoc20::day12::Part2(ss2) << std::endl;
}
