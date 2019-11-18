#include <iostream>

#include <range/v3/all.hpp>

#include "helpers.h"

using namespace ranges;

int main(int argc, char *argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }

  auto steps = helpers::read_file(argv[1]);
  auto floors = steps
              | view::transform([](char c) { return (c == '(' ? 1 : -1); })
              | view::partial_sum()
              | to_vector;
  auto final_floor = floors[floors.size() - 1];
  auto first_base =
      1u + distance(floors | view::take_while([](auto x) { return x >= 0; }));

  std::cout << "Final floor: " << final_floor << '\n';
  std::cout << "Basement after: " << first_base << '\n';
}
