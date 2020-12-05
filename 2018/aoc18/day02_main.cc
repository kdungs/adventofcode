#include <iostream>
#include <string>
#include <vector>

#include "aoc18/day02.h"

int main(int argc, char *argv[]) {
  // Read puzzle input.
  std::vector<std::string> box_ids = {};
  std::string line;
  while (std::getline(std::cin, line)) {
    box_ids.push_back(line);
  }

  std::cout << "Part 1: " << aoc18::day02::Checksum(box_ids) << '\n';
  std::cout << "Part 2: "
            << aoc18::day02::FindCommonLettersOfCorrectBoxIds(box_ids) << '\n';
}
