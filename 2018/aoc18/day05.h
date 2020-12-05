#ifndef AOC18_DAY05_H_
#define AOC18_DAY05_H_

#include <list>
#include <string>

namespace aoc18 {
namespace day05 {

enum class Polarity { UP, DOWN };

struct Unit {
  const char identity;
  const Polarity polarity;

  bool operator==(const Unit& other) const {
    return (identity == other.identity) && (polarity == other.polarity);
  }
};

Unit MakeUnit(const char c);
bool DoReact(const Unit lhs, const Unit rhs);

using ReactionChain = std::list<Unit>;

ReactionChain BuildChain(const std::string& str);
ReactionChain ResolveChain(ReactionChain chain);

int FindShortestVariationLength(const ReactionChain& chain);

}  // namespace day05
}  // namespace aoc18

#endif  // AOC18_DAY05_H_
