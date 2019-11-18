#include <iostream>
#include <fstream>

#include <range/v3/all.hpp>

auto memory_length(const std::string& str) {
  auto l = 0u;
  auto it = std::begin(str);
  while (it != std::end(str)) {
    const char c = *it;
    if (c != '"') {
        ++l;
        if (c != '\\') {} else {
          ++it;
          const char nc = *it;
          if (nc == 'x') {
            ++it;
            ++it;
          }
      }
    }
    ++it;
  }
  return l;
}

auto encoded_length(const std::string& str) {
  auto l = 2u;
  for (char c : str) {
    if (c == '\\' || c == '"' || c == '\'') {
      ++l;
    }
    ++l;
  }
  return l;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  auto ifs = std::ifstream{argv[1]};
  auto lines = ranges::getlines(ifs) | ranges::to_vector;
  auto clen = ranges::accumulate(lines | ranges::view::transform([](const auto& line) { return line.size(); }), 0u);
  auto mlen = ranges::accumulate(lines | ranges::view::transform(&memory_length), 0u);
  auto elen = ranges::accumulate(lines | ranges::view::transform(&encoded_length), 0u);

  std::cout << (clen - mlen) << '\n';
  std::cout << (elen - clen) << '\n';
}
