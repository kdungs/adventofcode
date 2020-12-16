#include <iostream>
#include <string>

#include "aoc20/day16.h"

int main(int argc, char *argv[]) {
  std::istreambuf_iterator<char> begin(std::cin), end;
  std::string inputstr(begin, end);

  auto maybe_input = aoc20::day16::ParseInput(inputstr);
  if (!maybe_input.has_value()) {
    std::cerr << "unable to parse input\n";
    return 1;
  }
  auto input = maybe_input.value();

  std::cout << "Part 1: " << aoc20::day16::Part1(input) << std::endl;
  std::cout << "Part 2: " << aoc20::day16::Part2(input) << std::endl;
}
