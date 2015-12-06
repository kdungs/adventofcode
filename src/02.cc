#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>

#include <range/v3/all.hpp>
#include <range/v3/numeric/accumulate.hpp>

#include "helpers.h"

unsigned calculate_material(unsigned l, unsigned w, unsigned h) {
  auto sides = std::vector<unsigned>{l * w, w * h, h * l};

  return *std::min_element(std::begin(sides), std::end(sides)) +
         std::accumulate(
             std::begin(sides), std::end(sides), 0u,
             [](const auto& acc, const auto& x) { return acc + 2 * x; });
}


int main(int argc, char* argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  using namespace ranges;

  auto data = helpers::read_file(argv[1]);
  auto view_line = view::split('x')
                 | view::transform([](std::string s) { return std::stoul(s); }); 
  auto split = data
             | view::split('\n')
             | view::transform(view_line)
             | view::transform([](auto rng) {
                 auto xs = rng | to_vector;
                 return calculate_material(xs[0], xs[1], xs[2]);
               });
  auto total = accumulate(split, 0u);
  
  std::cout << total << '\n';
}
