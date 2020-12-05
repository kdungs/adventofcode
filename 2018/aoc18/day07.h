#ifndef AOC18_DAY07_H_
#define AOC18_DAY07_H_

#include <functional>
#include <string>
#include <unordered_map>
#include <vector>

namespace aoc18 {
namespace day07 {

// Since the problem is limited to only characters from A-Z, we could micro
// optimize here and use char, bitset, and an array instead of these types.
using Node = std::string;
using AdjacencyList = std::vector<Node>;
using Graph = std::unordered_map<Node, AdjacencyList>;

Graph BuildDependencyGraph(const std::vector<std::string>& lines);

std::string Join(const std::vector<Node>& nodes);

std::vector<Node> Kahn(const Graph& graph);

// -------
// Part II
// -------

using TimerFunction = std::function<int(const Node&)>;

int NodeTime(const Node& node);

int TimeKahnWithWorkers(const TimerFunction& timer, const Graph& graph,
                        int num_workers);

}  // namespace day07
}  // namespace aoc18

#endif  // AOC18_DAY07_H_
