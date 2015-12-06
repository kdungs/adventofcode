#include <iostream>

#include <range/v3/core.hpp>
#include <range/v3/to_container.hpp>
#include <range/v3/view/partial_sum.hpp>
#include <range/v3/view/take_while.hpp>
#include <range/v3/view/transform.hpp>

#include "helpers.h"


int main(int argc, char *argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  using namespace ranges;

  auto steps = helpers::read_file(argv[1]);
  auto floors = steps
              | view::transform([](char c) { return (c == '(' ? 1 : -1); })
              | view::partial_sum()
              | to_vector;  // TODO: how to take last element of finite range?
  auto final_floor = floors[floors.size() - 1];
  auto first_base = 1u + distance(
      floors | view::take_while([](auto x) { return x >= 0; })
  );

  std::cout << "Final floor: " << final_floor << '\n';
  std::cout << "Basement after: " << first_base << '\n';
}
