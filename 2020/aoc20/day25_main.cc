#include <iostream>

#include "aoc20/day25.h"

int main(int argc, char *argv[]) {
  int pub_card;
  int pub_door;
  std::cin >> pub_card >> pub_door;

  std::cout << "Part 1: " << aoc20::day25::Part1(pub_card, pub_door)
            << std::endl;
}
