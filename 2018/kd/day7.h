#ifndef KD_DAY7_H_
#define KD_DAY7_H_

#include <string>
#include <unordered_map>
#include <vector>

namespace kd {
namespace day7 {

// Since the problem is limited to only characters from A-Z, we could micro
// optimize here and use char, bitset, and an array instead of these types.
using Node = std::string;
using AdjacencyList = std::vector<Node>;
using Graph = std::unordered_map<Node, AdjacencyList>;

Graph BuildDependencyGraph(const std::vector<std::string>& lines);

std::string Join(const std::vector<Node>& nodes);

std::vector<Node> Kahn(const Graph& graph);

}  // namespace day7
}  // namespace kd

#endif  // KD_DAY7_H_
