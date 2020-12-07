#include "aoc20/day07.h"

#include <utility>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

using ::testing::SizeIs;
using ::testing::UnorderedElementsAre;

constexpr char kExampleRules[] =
    R"(light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
)";

TEST(Part1, CanParseRules) {
  std::stringstream ss{kExampleRules};
  auto maybe_rules = aoc20::day07::ParseRules(ss);
  EXPECT_TRUE(maybe_rules.has_value());
  auto rules = maybe_rules.value();
  EXPECT_EQ(9, rules.size());
  // check contents
  EXPECT_THAT(rules["light red"],
              UnorderedElementsAre(std::make_pair("bright white", 1),
                                   std::make_pair("muted yellow", 2)));
  EXPECT_THAT(rules["dark orange"],
              UnorderedElementsAre(std::make_pair("bright white", 3),
                                   std::make_pair("muted yellow", 4)));
  EXPECT_THAT(rules["bright white"],
              UnorderedElementsAre(std::make_pair("shiny gold", 1)));
  EXPECT_THAT(rules["muted yellow"],
              UnorderedElementsAre(std::make_pair("shiny gold", 2),
                                   std::make_pair("faded blue", 9)));
  EXPECT_THAT(rules["shiny gold"],
              UnorderedElementsAre(std::make_pair("dark olive", 1),
                                   std::make_pair("vibrant plum", 2)));
  EXPECT_THAT(rules["dark olive"],
              UnorderedElementsAre(std::make_pair("faded blue", 3),
                                   std::make_pair("dotted black", 4)));
  EXPECT_THAT(rules["vibrant plum"],
              UnorderedElementsAre(std::make_pair("faded blue", 5),
                                   std::make_pair("dotted black", 6)));
  EXPECT_THAT(rules["faded blue"], SizeIs(0));
  EXPECT_THAT(rules["dotted black"], SizeIs(0));
}

TEST(Part1, WorksForExamples) {
  std::stringstream ss{kExampleRules};
  auto maybe_rules = aoc20::day07::ParseRules(ss);
  ASSERT_TRUE(maybe_rules.has_value());
  auto rules = maybe_rules.value();
  EXPECT_EQ(4, aoc20::day07::Part1(rules));
}

TEST(Part2, WorksForExamples) {
  std::stringstream ss{kExampleRules};
  auto maybe_rules = aoc20::day07::ParseRules(ss);
  ASSERT_TRUE(maybe_rules.has_value());
  auto rules = maybe_rules.value();
  EXPECT_EQ(32, aoc20::day07::Part2(rules));
}

TEST(Part2, WorksForOtherExample) {
  std::stringstream ss{R"(shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
)"};
  auto maybe_rules = aoc20::day07::ParseRules(ss);
  ASSERT_TRUE(maybe_rules.has_value());
  auto rules = maybe_rules.value();
  EXPECT_EQ(126, aoc20::day07::Part2(rules));
}
