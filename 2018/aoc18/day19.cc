#include "aoc18/day19.h"

#include <array>
#include <functional>
#include <iostream>
#include <optional>

#include "absl/strings/numbers.h"
#include "absl/strings/str_split.h"
#include "absl/strings/string_view.h"

namespace aoc18 {
namespace day19 {

using OpFn = std::function<Reg(Reg, Num, Num, Num)>;

static const std::array<OpFn, N_OP> OPS{
    /*ADDR=*/[](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = r[a] + r[b];
      return r;
    },
    /*ADDI=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = r[a] + b;
      return r;
    },
    /*MULR=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = r[a] * r[b];
      return r;
    },
    /*MULI=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = r[a] * b;
      return r;
    },
    /*BANR=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = r[a] & r[b];
      return r;
    },
    /*BANI=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = r[a] & b;
      return r;
    },
    /*BORR=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = r[a] | r[b];
      return r;
    },
    /*BORI=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = r[a] | b;
      return r;
    },
    /*SETR=*/
    [](Reg r, Num a, Num _, Num c) -> Reg {
      r[c] = r[a];
      return r;
    },
    /*SETI=*/
    [](Reg r, Num a, Num _, Num c) -> Reg {
      r[c] = a;
      return r;
    },
    /*GTIR=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = a > r[b];
      return r;
    },
    /*GTRI=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = r[a] > b;
      return r;
    },
    /*GTRR=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = r[a] > r[b];
      return r;
    },
    /*EQIR=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = a == r[b];
      return r;
    },
    /*EQRI=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = r[a] == b;
      return r;
    },
    /*EQRR=*/
    [](Reg r, Num a, Num b, Num c) -> Reg {
      r[c] = r[a] == r[b];
      return r;
    }};

Reg Run(const Prog& prog, Reg r) {
  Num& ip = r[prog.ip];
  while (ip >= 0 && ip < prog.ins.size()) {
    const Ins& i = prog.ins[ip];
    r = OPS[i.op](r, i.a, i.b, i.c);
    ++ip;
  }
  return r;
}

std::optional<Op> ParseOp(std::string_view s) {
  if (s == "addr") {
    return ADDR;
  } else if (s == "addi") {
    return ADDI;
  } else if (s == "mulr") {
    return MULR;
  } else if (s == "muli") {
    return MULI;
  } else if (s == "banr") {
    return BANR;
  } else if (s == "bani") {
    return BANI;
  } else if (s == "borr") {
    return BORR;
  } else if (s == "bori") {
    return BORI;
  } else if (s == "setr") {
    return SETR;
  } else if (s == "seti") {
    return SETI;
  } else if (s == "gtir") {
    return GTIR;
  } else if (s == "gtri") {
    return GTRI;
  } else if (s == "gtrr") {
    return GTRR;
  } else if (s == "eqir") {
    return EQIR;
  } else if (s == "eqri") {
    return EQRI;
  } else if (s == "eqrr") {
    return EQRR;
  }
  return std::nullopt;
}

std::optional<Ins> ParseIns(absl::string_view s) {
  std::vector<absl::string_view> parts = absl::StrSplit(s, ' ');
  if (parts.size() != 4) {
    return std::nullopt;
  }
  auto maybe_op = ParseOp(parts[0]);
  if (!maybe_op.has_value()) {
    return std::nullopt;
  }
  Ins ins{.op = maybe_op.value()};
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

std::optional<std::size_t> ParseIp(absl::string_view s) {
  // Remove "#ip "
  s.remove_prefix(4);
  std::size_t ip;
  if (!absl::SimpleAtoi(s, &ip)) {
    return std::nullopt;
  }
  return ip;
}

std::optional<Prog> ParseProg(const std::string& input) {
  std::vector<absl::string_view> lines =
      absl::StrSplit(input, '\n', absl::SkipEmpty());
  auto maybe_ip = ParseIp(lines[0]);
  if (!maybe_ip.has_value()) {
    return std::nullopt;
  }
  Prog prog{.ip = maybe_ip.value()};
  for (auto it = lines.begin() + 1; it != lines.end(); ++it) {
    auto maybe_ins = ParseIns(*it);
    if (!maybe_ins.has_value()) {
      return std::nullopt;
    }
    prog.ins.push_back(maybe_ins.value());
  }
  return prog;
}

int Part1(const Prog& prog) {
  auto res = Run(prog, Reg{0, 0, 0, 0, 0, 0});
  return res[0];
}

int Part2(const Prog& prog) {
  // Part two is a somewhat longer adventure. First, I attempted to run the
  // code as in part 1, just with Reg{1, 0, ...}. Unfortunately, this doesn't
  // really seem to terminate in an acceptable time.
  // So I looked at the assembly…

  // First, I translated the assembly code to an assignment syntax, that made
  // it easier for me to understand what was going on.
  //
  //   0: ip = ip + 16   -- goto 17
  //   1: r5 = 1
  //   2: r4 = 1
  //   3: r2 = r5 * r4
  //   4: r2 = r2 == r1  -- if (r2 == r1) {
  //   5: ip = r2 + ip   --   goto 7 } else {
  //   6: ip = ip + 1    --   goto 8 }
  //   7: r0 = r5 + r0
  //   8: r4 = r4 + 1
  //   9: r2 = r4 > r1   -- if (r4 > r1) {
  //  10: ip = ip + r2   --   goto 12 } else {
  //  11: ip = 2         --   goto 3 }
  //  12: r5 = r5 + 1
  //  13: r2 = r5 > r1   -- if (r5 > r1) {
  //  14: ip = r2 + ip   --   goto 16 } else {
  //  15: ip = 1         --   goto 2 }
  //  16: ip = ip * ip   --   goto end (16 * 16 = 255)
  //  17: r1 = r1 + 2
  //  18: r1 = r1 * r1
  //  19: r1 = ip * r1
  //  20: r1 = r1 * 11
  //  21: r2 = r2 + 2
  //  22: r2 = r2 * ip
  //  23: r2 = r2 + 20
  //  24: r1 = r1 + r2
  //  25: ip = ip + r0  -- goto r0 + 25 + 1
  //  26: ip = 0        -- (if r0 == 0) goto 1
  //  27: r2 = ip
  //  28: r2 = r2 * ip
  //  29: r2 = ip + r2
  //  30: r2 = ip * r2
  //  31: r2 = r2 * 14
  //  32: r2 = r2 * ip
  //  33: r1 = r1 + r2
  //  34: r0 = 0
  //  35: ip = 0        -- goto 1

  // Then, translated this to C code and gradually simplified it. At the end, I
  // was left with the following program.
  //  #include <stdio.h>
  //
  //  int main() {
  //    int r0 = 0;
  //    int r1 = 0;
  //    int r2 = 0;
  //    int r4 = 0;
  //    int r5 = 0;
  //
  //    // This block starts on line 17 and is only executed once. It
  //    // initializes r1 and r2 depending on whether r0 is set to 0 (part 1)
  //    // or 1 (part 2).
  //
  //    r1 += 2;
  //    r1 = (r1 * r1) * 19 * 11;
  //    r2 = (r2 + 2) * 22 + 20;
  //    r1 += r2;
  //    if (r0) {
  //      r2 = (27 * 28 + 29) * 30 * 14 * 32;
  //      r1 += r2;
  //      r0 = 0;
  //    }
  //    // At this point, r1 contains a large number that will be relevant
  //    // further down. It's 900 for part 1 and 10551300 for part 2.
  //
  //    // This is the actual program logic starting from line 1. Down here,
  //    // r2 is only used to evaluate booleans so we can simplify all
  //    // expressions that contain it. The resulting code with gotos looks
  //    // like this
  //    //    r5 = 1;
  //    //  line2:  // line 2
  //    //    r4 = 1;
  //    //  line3:  // line 3
  //    //    if ((r4 * r5) == r1) {
  //    //      r0 += r5;
  //    //    }
  //    //    r4 += 1;
  //    //    if (r4 <= r1) {
  //    //      goto line3;
  //    //    }
  //    //    r5 += 1;
  //    //    if (r5 <= r1) {
  //    //      goto line2;
  //    //    }
  //    // which can be further simplified into two nested for loops.
  //
  //    for (r5 = 1; r5 <= r1; r5++) {
  //      for (r4 = 1; r4 <= r1; r4++) {
  //        if (r4 * r5 == r1) {
  //          r0 += r5;
  //        }
  //      }
  //    }
  //    printf("%d\n", r0);
  //  }
  //
  // So what this code actually does is it calculates the sum of all the
  // divisors of our number. In order to get the stars, we can do that faster
  // here. So we run the program for a few steps to guarantee our r1 has been
  // calculated (afterwards it doesn't change anymore for the rest of the
  // program).

  auto RunSteps = [](const Prog& prog, Reg r, int n) -> Reg {
    Num& ip = r[prog.ip];
    for (int s{0}; s < n; ++s) {
      const Ins& i = prog.ins[ip];
      r = OPS[i.op](r, i.a, i.b, i.c);
      ++ip;
    }
    return r;
  };
  Reg r{1, 0, 0, 0, 0, 0};
  Reg res = RunSteps(prog, r, 10000);

  int num = res[1];
  int sum{num};
  for (int i{1}; i <= num / 2; ++i) {
    if (num % i == 0) {
      sum += i;
    }
  }
  return sum;
}

}  // namespace day19
}  // namespace aoc18
