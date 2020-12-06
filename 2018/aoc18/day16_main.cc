#include "aoc18/day16.h"

#include <iostream>
#include <string>
#include <vector>

int main(int argc, char* argv[]) {
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(std::cin, line)) {
    lines.push_back(line);
  }

  const auto maybe_samples = aoc18::day16::ParseSamples(lines);
  if (!maybe_samples.has_value()) {
    std::cerr << "unable to parse samples" << std::endl;
    return 1;
  }
  std::cout << "Part 1: " << aoc18::day16::Part1(maybe_samples.value()) << std::endl;
}
