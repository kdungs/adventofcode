#include <fstream>
#include <iostream>
#include <streambuf>
#include <vector>

#include <range/v3/core.hpp>
#include <range/v3/istream_range.hpp>
#include <range/v3/to_container.hpp>

#include <range/v3/view/partial_sum.hpp>
#include <range/v3/view/take_while.hpp>
#include <range/v3/view/transform.hpp>

using namespace ranges;

auto read_file(const char* filename) {
  auto ifs = std::ifstream(filename);
  return std::vector<char>(std::istreambuf_iterator<char>(ifs),
                           std::istreambuf_iterator<char>{});
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  auto steps = read_file(argv[1]);
  auto floors = steps
              | view::transform([](char c) { return (c == '(' ? 1 : -1); })
              | view::partial_sum()
              | to_vector;
  auto final_floor = floors[floors.size() - 1];
  auto first_base =
      1 + distance(floors | view::take_while([](auto x) { return x >= 0; }));

  std::cout << "Final floor: " << final_floor << '\n';
  std::cout << "Basement after: " << first_base << '\n';
}
