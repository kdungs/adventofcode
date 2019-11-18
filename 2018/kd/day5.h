#ifndef KD_DAY5_H_
#define KD_DAY5_H_

#include <list>
#include <string>

namespace kd {
namespace day5 {

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

}  // namespace day5
}  // namespace kd

#endif  // KD_DAY5_H_
