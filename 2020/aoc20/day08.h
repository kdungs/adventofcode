#ifndef AOC20_DAY08_H_
#define AOC20_DAY08_H_

#include <istream>
#include <optional>
#include <vector>

namespace aoc20 {
namespace day08 {

enum class Op { NOP, ACC, JMP };

struct Ins {
  Op op;
  int x;

  bool operator==(const Ins& other) const {
    return op == other.op && x == other.x;
  }
};

using Prog = std::vector<Ins>;

std::optional<Prog> ParseProg(std::istream& in);

struct VM {
  int ip;  // instruction pointer
  int acc;

  // Returns false if ip is outside program bounds.
  bool Step(const Prog& prog) {
    if (ip < 0 || ip >= prog.size()) {
      return false;
    }
    auto ins = prog[ip];
    if (ins.op == Op::NOP) {
      ++ip;
    } else if (ins.op == Op::ACC) {
      acc += ins.x;
      ++ip;
    } else if (ins.op == Op::JMP) {
      ip += ins.x;
    }
    return true;
  }
};

int Part1(const Prog& prog);

int Part2(const Prog& prog);

}  // namespace day08
}  // namespace aoc20

#endif  // AOC20_DAY08_H_
