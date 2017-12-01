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

int SolvePart1(const std::string& x) {
  return ProcessPair(x.front(), x.back()) +
         std::inner_product(std::begin(x), std::end(x) - 1,
                            std::begin(x) + 1, 0, std::plus<int>(),
                            ProcessPair);
}

int SolvePart2(const std::string& x) {
  const int size = x.size();
  const std::string ds = x + x;
  return std::inner_product(std::begin(ds), std::begin(ds) + size,
                            std::begin(ds) + (size / 2), 0, std::plus<int>(),
                            ProcessPair);
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  const std::string input(argv[1]);
  std::cout << SolvePart1(input) << '\n';
  std::cout << SolvePart2(input) << '\n';
}
