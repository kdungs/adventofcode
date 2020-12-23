#include "aoc20/day23.h"

#include <algorithm>
#include <cassert>
#include <iostream>

namespace aoc20 {
namespace day23 {
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

Node* FindNext(std::vector<Node>& nodes, Node* cur, int max) {
  NextNumberGenerator nng{cur->val, max};
  const Node* a = cur->next;
  const Node* b = a->next;
  const Node* c = b->next;
  int val = nng.Next();
  while (val == a->val || val == b->val || val == c->val) {
    val = nng.Next();
  }
  return &nodes[val - 1];
}

// Produces a vector of nodes that are sorted such that their index in the
// vector corresponds to their value - 1. Additionally, the nodes form a doubly
// linked list according to the order given by input.
std::vector<Node> NewGame(const std::vector<int>& input, std::size_t nums) {
  std::vector<Node> nodes(nums, Node{0});
  for (int i{0}; i < nums; ++i) {
    nodes[i].val = i + 1;
    nodes[i].prev = &nodes[(nums + i - 1) % nums];
    nodes[i].next = &nodes[(i + 1) % nums];
  }
  // Correct the pointers of the input nodes.
  Node* nxt = &nodes[input.size() % nums];
  Node* end = &nodes[nums - 1];
  if (nums == input.size()) {
    nxt = &nodes[input[0] - 1];
    end = &nodes[input[input.size() - 1] - 1];
  }
  for (int i{0}; i < input.size(); ++i) {
    int val = input[i];
    Node* n = &nodes[val - 1];
    if (i == 0) {
      n->prev = end;
      end->next = n;
    } else {
      n->prev = &nodes[input[i - 1] - 1];
    }
    if (i == input.size() - 1) {
      n->next = nxt;
      nxt->prev = n;
    } else {
      n->next = &nodes[input[i + 1] - 1];
    }
  }
  return nodes;
}

std::vector<Node> Play(const std::vector<int>& input, std::size_t nums,
                       std::size_t rounds) {
  std::vector<Node> nodes = NewGame(input, nums);
  Node* cur = &nodes[input[0] - 1];
  for (std::size_t round{0}; round < rounds; ++round) {
    Node* nxt = FindNext(nodes, cur, nums);
    MoveLifted(cur, nxt);
    cur = cur->next;
  }
  return nodes;
}

std::string Part1(const std::vector<int>& input) {
  std::vector<Node> nodes = Play(input, 9, 100);
  // Find the one labelled 1 and build a string with all the nodes after it.
  Node* one = &nodes[0];
  Node* it = one->next;
  std::string result;
  while (it != one) {
    result += std::to_string(it->val);
    it = it->next;
  }

  return result;
}

int64_t Part2(const std::vector<int>& input) {
  std::vector<Node> nodes = Play(input, 1000000, 10000000);
  Node* one = &nodes[0];
  return static_cast<int64_t>(one->next->val) * one->next->next->val;
}

}  // namespace day23
}  // namespace aoc20
