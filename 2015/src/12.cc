#include <regex>
#include <iostream>
#include <numeric>

#include "helpers.h"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  auto data = helpers::read_file<std::string>(argv[1]);
  const auto r = std::regex{"(-?\\d+)"};

  std::cout << std::accumulate(
                   std::sregex_iterator(std::begin(data), std::end(data), r),
                   std::sregex_iterator{}, 0ll,
                   [](auto acc, auto match) {
                     return acc + std::stol(match.str());
                   })
            << '\n';
}
