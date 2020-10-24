#include <iostream>

#include "kd/day9.h"

int main(int argc, char *argv[]) {
  std::string input;
  std::getline(std::cin, input);
  const auto maybe_input = kd::day9::ParseInput(input);
  if (!maybe_input.has_value()) {
    std::cerr << "invalid input: " << input << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << kd::day9::HighScore(maybe_input.value()) << std::endl;

  kd::day9::Input p2in = maybe_input.value();
  p2in.max_marble *= 100;
  std::cout << kd::day9::HighScore(p2in) << std::endl;
}
