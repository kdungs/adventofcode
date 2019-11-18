#include <functional>
#include <iostream>
#include <numeric>
#include <string>

int ProcessPair(const char a, const char b) {
  if (a != b) {
    return 0;
  }
  return a - '0';
}

int Solve(const std::string& x, const int offset) {
  const int size = x.size();
  const std::string ds = x + x;
  const auto left = std::begin(ds);
  return std::inner_product(left, left + size, left + offset, 0,
                            std::plus<int>(), ProcessPair);
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  const std::string input(argv[1]);
  std::cout << Solve(input, 1) << '\n';
  std::cout << Solve(input, input.size() / 2) << '\n';
}
