#include <iostream>
#include <string>

#include "aoc18/day05.h"

int main(int argc, char *argv[]) {
  std::string input;
  std::cin >> input;
  const auto chain =
      aoc18::day05::ResolveChain(aoc18::day05::BuildChain(input));

  std::cout << "Part 1: " << chain.size() << '\n';
  std::cout << "Part 2: " << aoc18::day05::FindShortestVariationLength(chain)
            << '\n';
}
