#include <algorithm>
#include <iostream>
#include <map>
#include <vector>

std::vector<int> Redistribute(std::vector<int> banks) {
  const int size = banks.size();
  int index = std::distance(
      std::begin(banks), std::max_element(std::begin(banks), std::end(banks)));
  int blocks = banks[index];
  banks[index] = 0;
  while (blocks > 0) {
    index = (index + 1) % size;
    --blocks;
    ++banks[index];
  }
  return banks;
}

std::pair<int, int> Solve(const std::vector<int>& input) {
  std::map<std::vector<int>, int> solutions {{input, 0}};
  std::vector<int> current = input;
  int steps = 0;
  auto it = std::end(solutions);
  while (true) {
    ++steps;
    current = Redistribute(current);
    it = solutions.find(current);
    if (it != std::end(solutions)) {
      return std::make_pair(steps, steps - it->second);
    }
    solutions.emplace(current, steps);
  }
}

int main(int argc, char* argv[]) {
  std::vector<int> input;
  int value;
  while (std::cin >> value) {
    input.push_back(value);
  }
  const auto solutions = Solve(input);
  std::cout << solutions.first << '\n';
  std::cout << solutions.second << '\n';
}
