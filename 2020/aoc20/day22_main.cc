#include <iostream>

#include "aoc20/day22.h"

int main(int argc, char *argv[]) {
  aoc20::day22::Game game;
  std::cin >> game;

  std::cout << "Part 1: " << aoc20::day22::Part1(game) << std::endl;
  std::cout << "Part 2: " << aoc20::day22::Part2(game) << std::endl;
}
