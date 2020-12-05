#ifndef AOC18_DAY09_H_
#define AOC18_DAY09_H_

#include <optional>
#include <string>

namespace aoc18 {
namespace day09 {

struct Input {
  int num_players;
  int max_marble;
};

std::optional<Input> ParseInput(const std::string& input);

std::uint64_t HighScore(const Input& in);

}  // namespace day09
}  // namespace aoc18

#endif  // AOC18_DAY09_H_
