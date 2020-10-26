#include "kd/day4.h"

#include "absl/strings/str_split.h"
#include "gtest/gtest.h"

using kd::day4::ParseShiftPlan;
using kd::day4::SolvePart1;
using kd::day4::SolvePart2;

constexpr char kInput[] = R"([1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up)";

TEST(Part1, WorksForExample) {
  auto lines = absl::StrSplit(kInput, '\n');
  auto maybe_shifts = ParseShiftPlan(lines);
  ASSERT_TRUE(maybe_shifts.has_value());

  EXPECT_EQ(240, SolvePart1(maybe_shifts.value()));
}

TEST(Part2, WorksForExample) {
  auto lines = absl::StrSplit(kInput, '\n');
  auto maybe_shifts = ParseShiftPlan(lines);
  ASSERT_TRUE(maybe_shifts.has_value());

  EXPECT_EQ(4455, SolvePart2(maybe_shifts.value()));
}
