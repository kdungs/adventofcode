#include "kd/day7.h"

#include <functional>
#include <iostream>
#include <optional>
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
      R"(^Step ([A-Z]) must be finished before step ([A-Z]) can begin.$)");
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

int NodeTime(const Node& node) { return node[0] - 'A' + 1; }

class Worker {
 public:
  Worker() : timer_{0}, work_{std::nullopt} {}

  bool busy() const { return timer_ != 0; }

  void Start(int timer, Node&& work) {
    timer_ = timer;
    work_.emplace(std::forward<Node>(work));
  }

  std::optional<Node> Tick() {
    if (!busy()) {
      return std::nullopt;
    }
    --timer_;
    if (busy()) {
      return std::nullopt;
    }
    // Done with work
    std::optional<Node> result;
    std::swap(result, work_);
    return result;
  }

 public:
  int timer_;
  std::optional<Node> work_;
};

class Scheduler {
 public:
  explicit Scheduler(const Graph& graph) : graph_(&graph) {
    for (const auto& node_and_adj : graph) {
      for (const Node& node : node_and_adj.second) {
        ++incoming_[node];
      }
    }
    std::vector<Node> zero_incoming;
    for (const auto& node_and_adj : graph) {
      const Node& node = node_and_adj.first;
      if (incoming_[node] == 0) {
        zero_incoming.push_back(node);
      }
    }
    pq_ = PQ(zero_incoming.begin(), zero_incoming.end());
  }

  // Doesn't account for tasks that are still being worked on.
  bool Done() const { return seen_.size() == graph_->size(); }

  bool HasWork() const { return !pq_.empty(); }

  // Calling code has to make sure HasWork()
  Node GetWork() {
    Node n = pq_.top();
    pq_.pop();
    seen_.insert(n);
    return n;
  }

  void Finish(Node n) {
    const auto it_adj = graph_->find(n);
    if (it_adj == graph_->end()) {
      return;
    }
    for (const Node& n : it_adj->second) {
      int x = --incoming_[n];
      if (x == 0) {
        pq_.push(n);
      }
    }
  }

 private:
  const Graph* const graph_;
  std::unordered_map<Node, int> incoming_;
  std::unordered_set<Node> seen_;
  PQ pq_;
};

int TimeKahnWithWorkers(const TimerFunction& timer, const Graph& graph,
                        int num_workers) {
  int time = -1;
  std::vector<Worker> workers(num_workers);
  Scheduler sched(graph);
  bool any_worker_busy = false;
  while (!sched.Done() || any_worker_busy) {
    any_worker_busy = false;
    ++time;
    for (Worker& worker : workers) {
      auto maybe_finished = worker.Tick();
      if (maybe_finished) {
        sched.Finish(*maybe_finished);
      }
      if (worker.busy()) {
        any_worker_busy = true;
      } else if (sched.HasWork()) {
        Node work = sched.GetWork();
        int t = timer(work);
        worker.Start(t, std::move(work));
        any_worker_busy = true;
      }
    }
  }
  return time;
}

}  // namespace day7
}  // namespace kd
