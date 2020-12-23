#include "aoc20/day23.h"

#include <algorithm>
#include <cassert>
#include <iostream>

namespace aoc20 {
namespace day23 {
static constexpr std::size_t N = 9;

class NextNumberGenerator {
 public:
  explicit NextNumberGenerator(int x, int max) : cur_{x}, max_{max} {};

  int Next() {
    --cur_;
    if (cur_ < 1) {
      cur_ = max_;
    }
    return cur_;
  }

 private:
  int cur_;
  int max_;
};

struct Node {
  explicit Node(int val) : val{val}, next{nullptr}, prev{nullptr} {}

  int val;
  Node* next;
  Node* prev;
};

void MoveLifted(Node* cur, Node* nxt) {
  // Moves the three cups that were lifted up to their new place right behind
  // nxt.
  Node* l = cur->next;      // the leftmost lifted cup.
  Node* r = l->next->next;  // the rightmost lifted cup.

  // Move them out of their current position.
  cur->next = r->next;
  r->next->prev = cur;

  // And into the new one.
  r->next = nxt->next;
  nxt->next->prev = r;
  nxt->next = l;
  l->prev = nxt;
}

Node* FindNext(Node* cur, int max) {
  NextNumberGenerator nng{cur->val, max};
  Node* start = cur->next->next->next->next;
  // We don't have to try more than 4 times. Worst thing that can happen is
  // that all three next candidates are currently lifted up.
  for (std::size_t i{0}; i < 4; ++i) {
    int target = nng.Next();
    Node* it = start;
    while (it != cur) {
      if (it->val == target) {
        return it;
      }
      it = it->next;
    }
  }
  assert(false);  // we have to find a candidate
}

std::string Part1(const std::vector<int>& input) {
  constexpr std::size_t kRounds = 100;

  // Build circular linked list.
  // Here, can already remember the index of the node with value 1 so we don't
  // have to find it later. Its position in the _vector_ isn't going to change
  // even if it moves around in the list.
  std::size_t oneidx{0};
  std::vector<Node> nodes(N, Node{0});
  for (std::size_t i{0}; i < N; ++i) {
    if (input[i] == 1) {
      oneidx = i;
    }
    nodes[i].val = input[i];
    nodes[i].prev = &nodes[(N + i - 1) % N];
    nodes[i].next = &nodes[(i + 1) % N];
  }

  Node* cur = &nodes[0];
  for (std::size_t round{0}; round < kRounds; ++round) {
    Node* nxt = FindNext(cur, N);
    MoveLifted(cur, nxt);
    cur = cur->next;
  }

  // Find the one labelled 1 and build a string with all the nodes after it.
  Node* one = &nodes[oneidx];
  Node* it = one->next;
  std::string result;
  while (it != one) {
    result += std::to_string(it->val);
    it = it->next;
  }

  return result;
}

int64_t Part2(const std::vector<int>& input) {
  constexpr std::size_t kRounds = 10000000;
  constexpr std::size_t kNums = 1000000;
  std::size_t oneidx{0};
  std::vector<Node> nodes(kNums, Node{0});
  for (std::size_t i{0}; i < kNums; ++i) {
    int val = i + 1;
    if (i < input.size()) {
      val = input[i];
      if (val == 1) {
        oneidx = i;
      }
    }
    nodes[i].val = val;
    nodes[i].prev = &nodes[(kNums + i - 1) % kNums];
    nodes[i].next = &nodes[(i + 1) % kNums];
  }

  Node* cur = &nodes[0];
  for (std::size_t round{0}; round < kRounds; ++round) {
    Node* nxt = FindNext(cur, kNums);
    MoveLifted(cur, nxt);
    cur = cur->next;
  }

  Node* one = &nodes[oneidx];
  return one->next->val * one->next->next->val;
}

}  // namespace day23
}  // namespace aoc20
