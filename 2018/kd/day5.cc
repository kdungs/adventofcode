#include "kd/day5.h"

#include <algorithm>
#include <cctype>
#include <iterator>
#include <string>

namespace kd {
namespace day5 {

Unit MakeUnit(const char c) {
  const char identity = std::tolower(c);
  const Polarity polarity = std::islower(c) ? Polarity::UP : Polarity::DOWN;
  return {identity, polarity};
}

bool IsZero(const Unit unit) { return unit.identity == '0'; }

bool DoReact(const Unit lhs, const Unit rhs) {
  return lhs.identity == rhs.identity && lhs.polarity != rhs.polarity;
}

ReactionChain BuildChain(const std::string& str) {
  ReactionChain chain;
  std::transform(std::begin(str), std::end(str), std::back_inserter(chain),
                 MakeUnit);
  return chain;
}

ReactionChain ResolveChain(ReactionChain chain) {
  auto it = std::begin(chain);
  auto nx = std::next(it);
  const auto end = std::end(chain);
  while (it != end && nx != end) {
    if (DoReact(*it, *nx)) {
      nx = chain.erase(it, std::next(nx));
      if (nx == std::begin(chain)) {
        it = nx;
        ++nx;
      } else {
        it = std::prev(nx);
      }
      if (it == nx) {
        ++nx;
      }
    } else {
      ++it;
      ++nx;
    }
  }
  return chain;
}

int FindShortestVariationLength(const ReactionChain& chain) {
  int shortest_length = chain.size();
  for (char c = 'a'; c <= 'z'; ++c) {
    ReactionChain reduced(chain);
    reduced.remove_if([c](const Unit& u) { return u.identity == c; });
    reduced = ResolveChain(reduced);
    if (reduced.size() < shortest_length) {
      shortest_length = reduced.size();
    }
  }
  return shortest_length;
}

}  // namespace day5
}  // namespace kd
