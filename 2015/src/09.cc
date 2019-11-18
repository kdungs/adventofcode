#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <regex>
#include <vector>

#include <range/v3/all.hpp>

struct DistanceMatrix {
  std::vector<std::size_t> dists;

  const auto n() const noexcept {
    return static_cast<std::size_t>(0.5 + std::sqrt(0.25 + 2 * dists.size()));
  }

  auto operator()(std::size_t l, std::size_t r) const noexcept {
    if (l > r) {
      std::swap(l, r);
    }
    const auto pos = l * n() + r - (l + 2) * (l + 1) / 2;
    return dists[pos];
  }

  void print(std::ostream& os) const noexcept {
    for (auto l = 0; l < n(); ++l) {
      for (auto r = l + 1; r < n(); ++r) {
        os << (*this)(l, r) << ' ';
      }
      std::cout << '\n';
    }
  }
};

auto shortest_and_longest_roundtrip(const DistanceMatrix& dists) {
  const auto n = dists.n();
  auto locations = std::vector<std::size_t>(n);
  std::iota(std::begin(locations), std::end(locations), 0u);
  auto minDist = 0u - 1;
  auto maxDist = 0u;
  do {
    auto distances = std::vector<std::size_t>{};
    locations.reserve(n - 1);
    std::adjacent_difference(std::begin(locations), std::end(locations),
                             std::back_inserter(distances),
                             [&dists](auto l, auto r) { return dists(l, r); });
    auto dist =
        std::accumulate(std::begin(distances) + 1, std::end(distances), 0u);
    if (dist < minDist) {
      minDist = dist;
    }
    if (dist > maxDist) {
      maxDist = dist;
    }
  } while (std::next_permutation(std::begin(locations), std::end(locations)));
  return std::make_pair(minDist, maxDist);
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  auto ifs = std::ifstream{argv[1]};
  auto lines = ranges::getlines(ifs) | ranges::to_vector;

  auto dists = std::vector<std::size_t>{};
  std::transform(std::begin(lines), std::end(lines), std::back_inserter(dists),
                 [](const auto& line) {
                   auto mat = std::smatch{};
                   std::regex_search(line, mat, std::regex{"(\\d+)"});
                   return std::stoul(mat[1].str());
                 });
  auto distances = shortest_and_longest_roundtrip(DistanceMatrix{dists});
  std::cout << distances.first << '\n';
  std::cout << distances.second << '\n';
}
