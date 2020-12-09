#include <iostream>
#include <vector>

#include "aoc20/day09.h"

int main(int argc, char *argv[]) {
  std::vector<int> input;
  int num;
  while (std::cin >> num) {
    input.push_back(num);
  }

  std::cout << "Part 1: " << aoc20::day09::Part1(input, 25) << std::endl;
  std::cout << "Part 2: " << aoc20::day09::Part2(input, 25) << std::endl;
}
