#include "kd/day2.h"

#include <iostream>
#include <string>
#include <vector>

int main(int argc, char *argv[]) {
  // Read puzzle input.
  std::vector<std::string> box_ids = {};
  std::string line;
  while (std::getline(std::cin, line)) {
    box_ids.push_back(line);
  }

  std::cout << "Part 1: " << kd::day2::Checksum(box_ids) << '\n';
  std::cout << "Part 2: " << kd::day2::FindCommonLettersOfCorrectBoxIds(box_ids)
            << '\n';
}
