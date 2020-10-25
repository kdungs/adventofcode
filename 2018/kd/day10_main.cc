#include <iostream>

#include "kd/day10.h"

using kd::day10::Body;
using kd::day10::FindSmallestConfiguration;
using kd::day10::Result;

int main(int argc, char *argv[]) {
  std::vector<Body> bodies;
  std::string line;
  while (std::getline(std::cin, line)) {
    std::optional<Body> maybe_body = kd::day10::ParseBody(line);
    if (!maybe_body.has_value()) {
      std::cerr << "could not parse '" << line << "'";
      return EXIT_FAILURE;
    }
    bodies.push_back(maybe_body.value());
  }

  Result res = FindSmallestConfiguration(bodies);
  std::cout << res.configuration << std::endl;
  std::cout << res.time << std::endl;
}
