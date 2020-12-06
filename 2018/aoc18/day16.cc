#include "aoc18/day16.h"

#include <algorithm>
#include <numeric>
#include <string>
#include <vector>

#include "absl/strings/numbers.h"
#include "absl/strings/str_split.h"

namespace aoc18 {
namespace day16 {

Registers AddR(Registers r, int a, int b, int c) {
  r[c] = r[a] + r[b];
  return r;
}
Registers AddI(Registers r, int a, int b, int c) {
  r[c] = r[a] + b;
  return r;
}
Registers MulR(Registers r, int a, int b, int c) {
  r[c] = r[a] * r[b];
  return r;
}
Registers MulI(Registers r, int a, int b, int c) {
  r[c] = r[a] * b;
  return r;
}
Registers BanR(Registers r, int a, int b, int c) {
  r[c] = r[a] & r[b];
  return r;
}
Registers BanI(Registers r, int a, int b, int c) {
  r[c] = r[a] & b;
  return r;
}
Registers BorR(Registers r, int a, int b, int c) {
  r[c] = r[a] | r[b];
  return r;
}
Registers BorI(Registers r, int a, int b, int c) {
  r[c] = r[a] | b;
  return r;
}
Registers SetR(Registers r, int a, int _, int c) {
  r[c] = r[a];
  return r;
}
Registers SetI(Registers r, int a, int _, int c) {
  r[c] = a;
  return r;
}
Registers GtIR(Registers r, int a, int b, int c) {
  r[c] = a > r[b];
  return r;
}
Registers GtRI(Registers r, int a, int b, int c) {
  r[c] = r[a] > b;
  return r;
}
Registers GtRR(Registers r, int a, int b, int c) {
  r[c] = r[a] > r[b];
  return r;
}
Registers EqIR(Registers r, int a, int b, int c) {
  r[c] = a == r[b];
  return r;
}
Registers EqRI(Registers r, int a, int b, int c) {
  r[c] = r[a] == b;
  return r;
}
Registers EqRR(Registers r, int a, int b, int c) {
  r[c] = r[a] == r[b];
  return r;
}

using Opcode = std::function<Registers(Registers, int, int, int)>;
constexpr std::size_t kNumOpcodes{16};
static const std::array<Opcode, kNumOpcodes> kOpcodes{
    AddR, AddI, MulR, MulI, BanR, BanI, BorR, BorI,
    SetR, SetI, GtIR, GtRI, GtRR, EqIR, EqRI, EqRR};

std::optional<Registers> ParseRegisters(const std::string& line) {
  static const std::regex re(R"(^\w+:\s+\[(\d+), (\d+), (\d+), (\d+)\]$)");
  std::smatch match;
  if (!std::regex_match(line, match, re)) {
    return std::nullopt;
  }
  if (match.size() != 5) {
    return std::nullopt;
  }
  Registers reg;
  if (!absl::SimpleAtoi(match[1].str(), &reg[0])) {
    return std::nullopt;
  }
  if (!absl::SimpleAtoi(match[2].str(), &reg[1])) {
    return std::nullopt;
  }
  if (!absl::SimpleAtoi(match[3].str(), &reg[2])) {
    return std::nullopt;
  }
  if (!absl::SimpleAtoi(match[4].str(), &reg[3])) {
    return std::nullopt;
  }
  return reg;
}

std::optional<Instruction> ParseInstruction(const std::string& line) {
  const std::vector<absl::string_view> parts = absl::StrSplit(line, ' ');
  if (parts.size() != 4) {
    return std::nullopt;
  }
  Instruction ins;
  if (!absl::SimpleAtoi(parts[0], &ins.code)) {
    return std::nullopt;
  }
  if (!absl::SimpleAtoi(parts[1], &ins.a)) {
    return std::nullopt;
  }
  if (!absl::SimpleAtoi(parts[2], &ins.b)) {
    return std::nullopt;
  }
  if (!absl::SimpleAtoi(parts[3], &ins.c)) {
    return std::nullopt;
  }
  return ins;
}

std::optional<Sample> ParseSample(const std::vector<std::string>& lines,
                                  const std::size_t position) {
  if (lines[position][0] != 'B') {
    // Not a sample.
    return std::nullopt;
  }
  const auto maybe_before = ParseRegisters(lines[position]);
  if (!maybe_before.has_value()) {
    return std::nullopt;
  }
  const auto maybe_instruction = ParseInstruction(lines[position + 1]);
  if (!maybe_before.has_value()) {
    return std::nullopt;
  }
  const auto maybe_after = ParseRegisters(lines[position + 2]);
  if (!maybe_after.has_value()) {
    return std::nullopt;
  }
  return Sample{.before = maybe_before.value(),
                .after = maybe_after.value(),
                .instruction = maybe_instruction.value()};
}

std::optional<SamplesAndProgram> ParseInput(
    const std::vector<std::string>& lines) {
  std::size_t idx{0};

  Samples samples;
  while (idx < lines.size()) {
    const auto maybe_sample = ParseSample(lines, idx);
    if (!maybe_sample.has_value()) {
      break;
    }
    samples.push_back(maybe_sample.value());
    idx += 4;
  }
  idx += 2;

  Program program;
  while (idx < lines.size()) {
    const auto maybe_instruction = ParseInstruction(lines[idx]);
    if (!maybe_instruction.has_value()) {
      return std::nullopt;
    }
    program.push_back(maybe_instruction.value());
    ++idx;
  }

  return std::make_tuple(samples, program);
}

bool IsPotentialOpcode(const Opcode& op, const Sample& s) {
  return op(s.before, s.instruction.a, s.instruction.b, s.instruction.c) ==
         s.after;
}

int CountPotentialOpcodes(const Sample& sample) {
  return std::count_if(
      kOpcodes.begin(), kOpcodes.end(),
      [&sample](const Opcode& op) { return IsPotentialOpcode(op, sample); });
}

int Part1(const Samples& samples) {
  return std::count_if(samples.begin(), samples.end(), [](const Sample& s) {
    return CountPotentialOpcodes(s) >= 3;
  });
}

uint16_t GetPotentialOpcodeIndices(const Sample& s) {
  uint16_t indices{0};
  for (std::size_t idx{0}; idx < kNumOpcodes; ++idx) {
    if (IsPotentialOpcode(kOpcodes[idx], s)) {
      indices |= (1 << idx);
    }
  }
  return indices;
}

std::array<Opcode, kNumOpcodes> FigureOutOpcodes(const Samples& samples) {
  std::unordered_map<int, std::vector<uint16_t>> hypotheses;
  for (const Sample& s : samples) {
    hypotheses[s.instruction.code].push_back(GetPotentialOpcodeIndices(s));
  }

  auto remove_hypothesis = [&hypotheses](uint16_t bits) {
    for (auto& [_, popidxs] : hypotheses) {
      for (uint16_t& pop : popidxs) {
        pop &= ~bits;
      }
    }
  };

  std::array<Opcode, kNumOpcodes> opcodes;
  while (hypotheses.size() > 0) {
    for (auto it = hypotheses.begin(); it != hypotheses.end();) {
      auto code = it->first;
      const auto& popidxs = it->second;
      auto pit = std::find_if(popidxs.begin(), popidxs.end(), [](uint16_t pop) {
        return __builtin_popcount(pop) == 1;
      });
      if (pit == popidxs.end()) {
        // Nothing interesting for this code, yet.
        ++it;
        continue;
      }
      uint16_t bits = *pit;
      auto idx = __builtin_ctz(bits);
      opcodes[code] = kOpcodes[idx];
      hypotheses.erase(it++);
      remove_hypothesis(bits);
    }
  }
  return opcodes;
}

Registers Run(const std::array<Opcode, kNumOpcodes>& opcodes,
              const Program& program) {
  Registers r;
  for (const auto& ins : program) {
    r = opcodes.at(ins.code)(r, ins.a, ins.b, ins.c);
  }
  return r;
}

int Part2(const Samples& samples, const Program& program) {
  auto opcs = FigureOutOpcodes(samples);
  auto res = Run(opcs, program);
  return res[0];
}

}  // namespace day16
}  // namespace aoc18
