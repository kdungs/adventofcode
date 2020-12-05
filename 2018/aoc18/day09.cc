#include "aoc18/day09.h"

#include <optional>
#include <regex>
#include <string>

namespace aoc18 {
namespace day09 {

std::optional<Input> ParseInput(const std::string& input) {
  static const std::regex re{
      R"(^(\d+) players; last marble is worth (\d+) points$)"};
  std::smatch match;
  if (!std::regex_match(input, match, re)) {
    return std::nullopt;
  }
  if (match.size() != 3) {
    return std::nullopt;
  }

  // This is challenge code; let's just be happy with uncaught exceptions...
  return Input{.num_players = std::stoi(match[1].str()),
               .max_marble = std::stoi(match[2].str())};
}

class MarbleCircle {
 public:
  MarbleCircle() : size_{1} {
    cur_ = new Node{0, nullptr, nullptr};
    cur_->prev = cur_;
    cur_->next = cur_;
  }

  ~MarbleCircle() {
    for (int i{0}; i < size_; ++i) {
      Node* n = cur_;
      cur_ = cur_->next;
      delete n;
    }
  }

  void Insert(int value) {
    ++size_;
    Node* before = cur_->next;
    Node* after = before->next;
    Node* inserted = new Node{value, before, after};
    before->next = inserted;
    after->prev = inserted;
    cur_ = inserted;
  }

  void MoveCounterClockwise(int steps) {
    for (int step{0}; step < steps; ++step) {
      cur_ = cur_->prev;
    }
  }

  // This would be problematic if we're removing the last element.
  // Luckily, this will never be an issue in the game so we don't deal with it
  // here.
  int Remove() {
    --size_;
    int value = cur_->value;
    Node* n = cur_;
    cur_ = n->next;
    n->prev->next = n->next;
    n->next->prev = n->prev;
    delete n;
    return value;
  }

 private:
  struct Node {
    int value;
    Node* prev;
    Node* next;
  };
  Node* cur_;
  std::size_t size_;
};

std::uint64_t HighScore(const Input& in) {
  MarbleCircle circle{};
  std::vector<std::uint64_t> scores(in.num_players, 0);
  int player = 0;
  for (int marble{1}; marble <= in.max_marble; ++marble) {
    if (marble % 23 == 0) {
      scores[player] += marble;
      circle.MoveCounterClockwise(7);
      int val = circle.Remove();
      scores[player] += val;
    } else {
      circle.Insert(marble);
    }
    // Next player
    player = (player + 1) % in.num_players;
  }

  return *std::max_element(scores.begin(), scores.end());
}

}  // namespace day09
}  // namespace aoc18
