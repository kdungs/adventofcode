#include <algorithm>
#include <iostream>
#include <sstream>
#include <vector>

auto num_to_string(unsigned long long num) {
  auto chars = std::vector<char>{};
  while (num > 0) {
    chars.emplace_back('0' + (num % 10));
    num /= 10;
  }
  std::reverse(std::begin(chars), std::end(chars));
  return std::string{std::begin(chars), std::end(chars)};
}

std::string transform(std::string cs) {
  std::stringstream ss;
  auto it = std::begin(cs);
  while (it != std::end(cs)) {
    const auto c = *it;
    auto pp = it;
    while (pp != std::end(cs) && *pp == *it) {
      ++pp;
    }
    const auto count = std::count(it, pp, c);
    ss << count << c;
    it = pp;
  }
  return ss.str();
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  auto num = std::stoull(argv[1]);
  auto cs = num_to_string(num);
  for (auto i = 0u; i < 40; ++i) {
    cs = transform(cs);
  }
  std::cout << cs.size() << '\n';
  for (auto i = 0u; i < 10; ++i) {
    cs = transform(cs);
  }
  std::cout << cs.size() << '\n';
}
