#include "aoc20/day02.h"

#include <algorithm>
#include <optional>
#include <regex>
#include <string>
#include <vector>

namespace aoc20 {
namespace day02 {

std::optional<Input> ParseInput(const std::string& line) {
  static std::regex re{R"(^(\d+)-(\d+) (\w): (\w+)$)"};

  std::smatch match;
  if (!std::regex_match(line, match, re)) {
    return std::nullopt;
  }
  if (match.size() != 5) {
    return std::nullopt;
  }

  return Input{
      .left = std::stoi(match[1]),
      .right = std::stoi(match[2]),
      .letter = match[3].str()[0],
      .password = match[4],
  };
}

std::optional<std::vector<Input>> ParseInputs(
    const std::vector<std::string>& lines) {
  std::vector<Input> inputs;
  inputs.reserve(lines.size());
  for (const std::string& line : lines) {
    auto maybe_input = ParseInput(line);
    if (!maybe_input.has_value()) {
      return std::nullopt;
    }
    inputs.push_back(maybe_input.value());
  }
  return inputs;
}

template <typename PasswordPolicy>
int CountValidPasswords(const std::vector<Input>& inputs,
                        PasswordPolicy is_valid_password) {
  return std::count_if(inputs.begin(), inputs.end(), is_valid_password);
}

bool Part1Policy(const Input& input) {
  auto count =
      std::count(input.password.begin(), input.password.end(), input.letter);
  return (input.left <= count) && (count <= input.right);
}

int Part1(const std::vector<Input>& inputs) {
  return CountValidPasswords(inputs, Part1Policy);
}

bool Part2Policy(const Input& input) {
  char at_left = input.password[input.left - 1];
  char at_right = input.password[input.right - 1];

  return (at_left == input.letter) != (at_right == input.letter);
}

int Part2(const std::vector<Input>& inputs) {
  return CountValidPasswords(inputs, Part2Policy);
}

}  // namespace day02
}  // namespace aoc20
