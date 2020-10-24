#ifndef KD_DAY9_H_
#define KD_DAY9_H_

#include <optional>
#include <string>

namespace kd {
namespace day9 {

struct Input {
  int num_players;
  int max_marble;
};

std::optional<Input> ParseInput(const std::string& input);

std::uint64_t HighScore(const Input& in);

}  // namespace day9
}  // namespace kd

#endif  // KD_DAY9_H_
