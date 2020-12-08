#include "aoc20/day08.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

constexpr char kExampleInput[] = R"(nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
)";

using aoc20::day08::Ins;
using aoc20::day08::Op;
using aoc20::day08::ParseProg;
using aoc20::day08::Part1;
using aoc20::day08::Part2;

namespace aoc20::day08 {
// This is not good style but it helps make the test error output more
// meaningful for now.
std::ostream& operator<<(std::ostream& os, const Ins& ins) {
  if (ins.op == Op::NOP) {
    os << "nop";
  } else if (ins.op == Op::ACC) {
    os << "acc";
  } else if (ins.op == Op::JMP) {
    os << "jmp";
  }
  os << " " << ins.x;
  return os;
}
}  // namespace aoc20::day08

TEST(Parser, CanParseProg) {
  std::stringstream ss{kExampleInput};
  auto maybe_prog = ParseProg(ss);
  EXPECT_TRUE(maybe_prog.has_value());
  auto prog = maybe_prog.value();

  EXPECT_THAT(prog, ::testing::ElementsAre(
                        Ins{Op::NOP, 0}, Ins{Op::ACC, 1}, Ins{Op::JMP, 4},
                        Ins{Op::ACC, 3}, Ins{Op::JMP, -3}, Ins{Op::ACC, -99},
                        Ins{Op::ACC, 1}, Ins{Op::JMP, -4}, Ins{Op::ACC, 6}));
}

TEST(Part1, WorksForExamples) {
  std::stringstream ss{kExampleInput};
  auto maybe_prog = ParseProg(ss);
  ASSERT_TRUE(maybe_prog.has_value());
  auto prog = maybe_prog.value();

  EXPECT_EQ(5, Part1(prog));
}

TEST(Part2, WorksForExamples) {
  std::stringstream ss{kExampleInput};
  auto maybe_prog = ParseProg(ss);
  ASSERT_TRUE(maybe_prog.has_value());
  auto prog = maybe_prog.value();

  EXPECT_EQ(8, Part2(prog));
}
