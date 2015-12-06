#include <algorithm>
#include <array>
#include <iostream>
#include <numeric>
#include <vector>

#include <range/v3/all.hpp>
#include <range/v3/numeric/accumulate.hpp>

#include "helpers.h"

struct Box {
  std::array<unsigned, 3> sides;
};

unsigned calculate_paper(const Box& box) {
  auto sides = ranges::view::zip_with(
                   std::multiplies<unsigned>{}, box.sides,
                   box.sides | ranges::view::cycle | ranges::view::drop(1))
             | ranges::to_vector;

  return *std::min_element(std::begin(sides), std::end(sides)) +
         std::accumulate(
             std::begin(sides), std::end(sides), 0u,
             [](const auto& acc, const auto& x) { return acc + 2 * x; });
}

unsigned calculate_ribbon(const Box& box) {
  auto sorted_sides = box.sides;
  sorted_sides |= ranges::action::sort;
  auto ribbon_box = 2 * (sorted_sides[0] + sorted_sides[1]);
  auto ribbon_bow =
      ranges::accumulate(box.sides, 1u, std::multiplies<unsigned>{});

  return ribbon_box + ribbon_bow;
}


int main(int argc, char* argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  using namespace ranges;

  auto data = helpers::read_file(argv[1]);
  auto view_line = view::split('x')
                 | view::transform([](std::string s) { return std::stoul(s); }); 
  auto boxes = data
             | view::split('\n')
             | view::transform(view_line)
             | view::transform([](auto rng) {
                 auto b = Box{};
                 copy(rng | view::take(3), std::begin(b.sides));
                 return b;
               });
  auto paper = accumulate(boxes | view::transform(calculate_paper), 0u);
  auto ribbon = accumulate(boxes | view::transform(calculate_ribbon), 0u);
  
  std::cout << "Paper: " << paper << " square feet\n";
  std::cout << "Ribbon: " << ribbon << " feet \n";
}
