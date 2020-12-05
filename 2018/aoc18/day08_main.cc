#include <iostream>
#include <vector>

#include "aoc18/day08.h"

int main(int argc, char* argv[]) {
  std::vector<int> tree;
  int val;
  while (std::cin >> val) {
    tree.push_back(val);
  }

  std::cout << aoc18::day08::SumMetadata(tree) << std::endl;

  std::cout << aoc18::day08::SumValues(tree) << std::endl;
}
