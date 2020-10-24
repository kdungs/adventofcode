#include <iostream>
#include <vector>

#include "kd/day8.h"

int main(int argc, char* argv[]) {
  std::vector<int> tree;
  int val;
  while (std::cin >> val) {
    tree.push_back(val);
  }

  std::cout << kd::day8::SumMetadata(tree) << std::endl;

  std::cout << kd::day8::SumValues(tree) << std::endl;
}
