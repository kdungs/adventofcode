#include <iostream>
#include <vector>

template <typename FN>
int Solve(std::vector<int> program, FN step) {
  const int size = program.size();
  int steps = 0;
  int addr = 0;
  while (addr >= 0 && addr < size) {
    ++steps;
    int& val = program[addr];
    addr += val;
    val = step(val);
  }
  return steps;
}

int main() {
  std::vector<int> input;
  int value;
  while (std::cin >> value) {
    input.push_back(value);
  }
  std::cout << Solve(input, [](const int val) { return val + 1; }) << '\n';
  std::cout << Solve(input, [](const int val) {
    if (val >= 3) {
      return val - 1;
    }
    return val + 1;
  }) << '\n';
}
