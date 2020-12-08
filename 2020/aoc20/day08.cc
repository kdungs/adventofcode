#include "aoc20/day08.h"

#include <iostream>
#include <istream>
#include <optional>
#include <string>
#include <tuple>
#include <vector>

#include "absl/container/flat_hash_set.h"
#include "absl/strings/numbers.h"
#include "absl/strings/str_split.h"
#include "absl/strings/string_view.h"

namespace aoc20 {
namespace day08 {

std::optional<Op> ParseOp(absl::string_view s) {
  if (s == "nop") {
    return Op::NOP;
  }
  if (s == "acc") {
    return Op::ACC;
  }
  if (s == "jmp") {
    return Op::JMP;
  }
  return std::nullopt;
}

std::optional<Ins> ParseIns(const std::string& line) {
  std::vector<absl::string_view> parts = absl::StrSplit(line, ' ');
  auto maybe_op = ParseOp(parts[0]);
  if (!maybe_op.has_value()) {
    return std::nullopt;
  }
  int x;
  if (!absl::SimpleAtoi(parts[1], &x)) {
    return std::nullopt;
  }
  return Ins{.op = maybe_op.value(), .x = x};
}

std::optional<Prog> ParseProg(std::istream& in) {
  Prog prog;
  std::string line;
  while (std::getline(in, line)) {
    auto maybe_ins = ParseIns(line);
    if (!maybe_ins.has_value()) {
      return std::nullopt;
    }
    prog.push_back(maybe_ins.value());
  }
  return prog;
}

int Part1(const Prog& prog) {
  absl::flat_hash_set<int> executed;
  VM vm{0, 0};
  while (!executed.contains(vm.ip)) {
    executed.insert(vm.ip);
    if (!vm.Step(prog)) {
      std::cerr << vm.ip << std::endl;
      throw std::runtime_error("unable to run program");
    }
  }
  return vm.acc;
}

std::tuple<bool, int> WillTerminate(const Prog& prog) {
  absl::flat_hash_set<int> executed;
  VM vm{0, 0};
  while (!executed.contains(vm.ip)) {
    executed.insert(vm.ip);
    if (!vm.Step(prog)) {
      return {true, vm.acc};
    }
  }
  return {false, vm.acc};
}

int Part2(const Prog& prog) {
  // First, we figure out which nops and jmps are actually executed. This is
  // equivalent to part 1.
  absl::flat_hash_set<int> executed;
  VM vm{0, 0};
  while (!executed.contains(vm.ip)) {
    executed.insert(vm.ip);
    if (!vm.Step(prog)) {
      std::cerr << vm.ip << std::endl;
      throw std::runtime_error("unable to run program");
    }
  }
  // At this point we have all the instructions but we want only nop and jmp.
  absl::flat_hash_set<int> nops_and_jmps;
  std::copy_if(executed.begin(), executed.end(),
               std::inserter(nops_and_jmps, nops_and_jmps.begin()),
               [&prog](int idx) { return prog[idx].op != Op::ACC; });
  // Try flipping each of them and see what happens
  for (int idx : nops_and_jmps) {
    Prog p{prog};
    Ins& i = p[idx];
    if (i.op == Op::NOP) {
      i.op = Op::JMP;
    } else if (i.op == Op::JMP) {
      i.op = Op::NOP;
    } else {
      throw std::runtime_error("unexpected op");
    }
    auto [term, acc] = WillTerminate(p);
    if (term) {
      return acc;
    }
  }
  return 0;
}

}  // namespace day08
}  // namespace aoc20
