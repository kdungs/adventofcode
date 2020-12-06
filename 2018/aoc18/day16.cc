#include "aoc18/day16.h"

#include <algorithm>
#include <string>
#include <vector>

#include "absl/strings/numbers.h"
#include "absl/strings/str_split.h"

namespace aoc18 {
namespace day16 {

using Opcode = std::function<Registers(Registers, int, int, int)>;
Registers AddR(Registers r, int a, int b, int c) { r[c] = r[a]  + r[b]; return r; }
Registers AddI(Registers r, int a, int b, int c) { r[c] = r[a]  +    b; return r; }
Registers MulR(Registers r, int a, int b, int c) { r[c] = r[a]  * r[b]; return r; }
Registers MulI(Registers r, int a, int b, int c) { r[c] = r[a]  *    b; return r; }
Registers BanR(Registers r, int a, int b, int c) { r[c] = r[a]  & r[b]; return r; }
Registers BanI(Registers r, int a, int b, int c) { r[c] = r[a]  &    b; return r; }
Registers BorR(Registers r, int a, int b, int c) { r[c] = r[a]  | r[b]; return r; }
Registers BorI(Registers r, int a, int b, int c) { r[c] = r[a]  |    b; return r; }
Registers SetR(Registers r, int a, int _, int c) { r[c] = r[a]        ; return r; }
Registers SetI(Registers r, int a, int _, int c) { r[c] =    a        ; return r; }
Registers GtIR(Registers r, int a, int b, int c) { r[c] =    a  > r[b]; return r; }
Registers GtRI(Registers r, int a, int b, int c) { r[c] = r[a]  >    b; return r; }
Registers GtRR(Registers r, int a, int b, int c) { r[c] = r[a]  > r[b]; return r; }
Registers EqIR(Registers r, int a, int b, int c) { r[c] =    a == r[b]; return r; }
Registers EqRI(Registers r, int a, int b, int c) { r[c] = r[a] ==    b; return r; }
Registers EqRR(Registers r, int a, int b, int c) { r[c] = r[a] == r[b]; return r; }


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
  if (!absl::SimpleAtoi(match[1].str(), &reg[0])) { return std::nullopt; }
  if (!absl::SimpleAtoi(match[2].str(), &reg[1])) { return std::nullopt; }
  if (!absl::SimpleAtoi(match[3].str(), &reg[2])) { return std::nullopt; }
  if (!absl::SimpleAtoi(match[4].str(), &reg[3])) { return std::nullopt; }
  return reg;
}

std::optional<Instruction> ParseInstruction(const std::string& line) {
  const std::vector<absl::string_view> parts = absl::StrSplit(line, ' ');
  if (parts.size() != 4) {
    return std::nullopt;
  }
  Instruction ins;
  if (!absl::SimpleAtoi(parts[0], &ins.code)) { return std::nullopt; }
  if (!absl::SimpleAtoi(parts[1], &ins.a)) { return std::nullopt; }
  if (!absl::SimpleAtoi(parts[2], &ins.b)) { return std::nullopt; }
  if (!absl::SimpleAtoi(parts[3], &ins.c)) { return std::nullopt; }
  return ins;
}

std::optional<std::vector<Sample>> ParseSamples(const std::vector<std::string>& lines) {
  std::vector<Sample> samples;
  int idx{0};
  while (idx < lines.size()) {
    if (lines[idx][0] != 'B') {
      // This means we're done parsing the section of the input that deals with
      // samples.
      break;
    }
    const auto maybe_before = ParseRegisters(lines[idx]);
    if (!maybe_before.has_value()) {
      return std::nullopt;
    }
    const auto maybe_instruction = ParseInstruction(lines[idx + 1]);
    if (!maybe_before.has_value()) {
      return std::nullopt;
    }
    const auto maybe_after = ParseRegisters(lines[idx + 2]);
    if (!maybe_after.has_value()) {
      return std::nullopt;
    }
    samples.emplace_back(Sample{
        .before = maybe_before.value(),
        .after = maybe_after.value(),
        .instruction = maybe_instruction.value()
    });
    idx += 4;
  }
  return samples;
}

int CountPotentialOpcodes(const Sample& sample) {
  static const std::vector<Opcode> ops{
    AddR,
    AddI,
    MulR,
    MulI,
    BanR,
    BanI,
    BorR,
    BorI,
    SetR,
    SetI,
    GtIR,
    GtRI,
    GtRR,
    EqIR,
    EqRI,
    EqRR
  };
  return std::count_if(
    ops.begin(),
    ops.end(),
    [&sample](const Opcode& op) {
      return op(
        sample.before,
        sample.instruction.a,
        sample.instruction.b,
        sample.instruction.c
      ) == sample.after;
    }
  );
}

int Part1(const std::vector<Sample>& samples) {
  return std::count_if(
    samples.begin(),
    samples.end(),
    [](const Sample& s) { return CountPotentialOpcodes(s) >= 3; }
  );
}

}  // namespace day16
}  // namespace aoc18
