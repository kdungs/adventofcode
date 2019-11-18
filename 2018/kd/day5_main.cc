#include "kd/day5.h"

#include <iostream>
#include <string>

int main(int argc, char *argv[]) {
  std::string input;
  std::cin >> input;
  const auto chain = kd::day5::ResolveChain(kd::day5::BuildChain(input));

  std::cout << "Part 1: " << chain.size() << '\n';
  std::cout << "Part 2: " << kd::day5::FindShortestVariationLength(chain)
            << '\n';
}
