#include <iostream>

#include "aoc18/day09.h"

int main(int argc, char *argv[]) {
  std::string input;
  std::getline(std::cin, input);
  const auto maybe_input = aoc18::day09::ParseInput(input);
  if (!maybe_input.has_value()) {
    std::cerr << "invalid input: " << input << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << aoc18::day09::HighScore(maybe_input.value()) << std::endl;

  aoc18::day09::Input p2in = maybe_input.value();
  p2in.max_marble *= 100;
  std::cout << aoc18::day09::HighScore(p2in) << std::endl;
}
