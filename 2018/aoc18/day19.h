#ifndef AOC18_DAY19_H_
#define AOC18_DAY19_H_

#include <array>
#include <optional>
#include <vector>

#include "absl/strings/string_view.h"

namespace aoc18 {
namespace day19 {

using Num = int;

constexpr std::size_t N_REG{6};
using Reg = std::array<Num, N_REG>;

enum Op : uint8_t {
  ADDR = 0x00,
  ADDI = 0x01,
  MULR = 0x02,
  MULI = 0x03,
  BANR = 0x04,
  BANI = 0x05,
  BORR = 0x06,
  BORI = 0x07,
  SETR = 0x08,
  SETI = 0x09,
  GTIR = 0x0A,
  GTRI = 0x0B,
  GTRR = 0x0C,
  EQIR = 0x0D,
  EQRI = 0x0E,
  EQRR = 0x0F
};
constexpr std::size_t N_OP{16};

struct Ins {
  Op op;
  Num a;
  Num b;
  Num c;
};

struct Prog {
  std::size_t ip;
  std::vector<Ins> ins;
};

std::optional<Prog> ParseProg(const std::string& input);

int Part1(const Prog& prog);

int Part2(const Prog& prog);

}  // namespace day19
}  // namespace aoc18

#endif  // AOC18_DAY19_H_
