#include "aoc20/day21.h"

#include <iterator>
#include <sstream>
#include <vector>

#include "gtest/gtest.h"

using aoc20::day21::Entry;
using aoc20::day21::Part1;
using aoc20::day21::Part2;

constexpr char kExampleInput[] =
    R"(mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
)";

std::vector<Entry> ExampleEntries() {
  std::stringstream ss{kExampleInput};
  return std::vector<Entry>(std::istream_iterator<Entry>{ss},
                            std::istream_iterator<Entry>{});
}

TEST(ParseEntry, WorksForExamples) { EXPECT_EQ(4, ExampleEntries().size()); }

TEST(Part1, WorksForExamples) { EXPECT_EQ(5, Part1(ExampleEntries())); }

TEST(Part2, WorksForExamples) {
  EXPECT_EQ("mxmxvkd,sqjhc,fvjkl", Part2(ExampleEntries()));
}
