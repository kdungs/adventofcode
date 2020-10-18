#include "kd/day7.h"

#include <functional>
#include <queue>
#include <regex>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "absl/strings/str_join.h"

namespace kd {
namespace day7 {

using PQ = std::priority_queue<Node, std::vector<Node>, std::greater<Node>>;

Graph BuildDependencyGraph(const std::vector<std::string>& lines) {
  static const std::regex re(
    R"(^Step ([A-Z]) must be finished before step ([A-Z]) can begin.$)"
  );
  Graph g;
  for (const auto& line : lines) {
    std::smatch match;
    if (std::regex_match(line, match, re)) {
      if (match.size() == 3) {
        const auto& first = match[1];
        const auto& then = match[2];
        g[first].push_back(then);
        g[then];  // so all nodes show up in the graph when iterating
      }
    }
  }
  return g;
}

std::string Join(const std::vector<std::string>& nodes) {
  return absl::StrJoin(nodes, "");
}

std::vector<Node> Kahn(const Graph& graph) {
  std::vector<Node> result;
  result.reserve(graph.size());

  std::unordered_map<Node, int> incoming;
  for (const auto& node_and_adj : graph) {
    for (const Node& node : node_and_adj.second) {
      ++incoming[node];
    }
  }

  std::vector<Node> zero_incoming;
  for (const auto& node_and_adj : graph) {
    const Node& node = node_and_adj.first;
    if (incoming[node] == 0) {
      zero_incoming.push_back(node);
    }
  }
  PQ pq(zero_incoming.begin(), zero_incoming.end());

  std::unordered_set<Node> seen;
  while (!pq.empty()) {
    const Node cur = pq.top();
    pq.pop();
    seen.insert(cur);
    result.push_back(cur);
    auto it = graph.find(cur);
    if (it == graph.end()) {
      continue;
    }
    for (const Node& node : it->second) {
      if (seen.count(node) == 0) {
        int x = --incoming[node];
        if (x == 0) {
          pq.push(node);
        }
      }
    }
  }

  return result;
}

}  // namespace day7
}  // namespace kd
