#include "kd/day5.h"

#include "gtest/gtest.h"

using kd::day5::BuildChain;
using kd::day5::DoReact;
using kd::day5::FindShortestVariationLength;
using kd::day5::MakeUnit;
using kd::day5::ReactionChain;

TEST(Unit, ReactionWorks) {
  // Only same identity and different polarity react.
  EXPECT_TRUE(DoReact(MakeUnit('a'), MakeUnit('A')));
  EXPECT_TRUE(DoReact(MakeUnit('A'), MakeUnit('a')));
  // Same identity but different polarity don't react.
  EXPECT_FALSE(DoReact(MakeUnit('a'), MakeUnit('a')));
  EXPECT_FALSE(DoReact(MakeUnit('A'), MakeUnit('A')));
  // Different identities don't react regardless of polarity.
  EXPECT_FALSE(DoReact(MakeUnit('a'), MakeUnit('b')));
  EXPECT_FALSE(DoReact(MakeUnit('a'), MakeUnit('B')));
  EXPECT_FALSE(DoReact(MakeUnit('A'), MakeUnit('b')));
  EXPECT_FALSE(DoReact(MakeUnit('A'), MakeUnit('B')));
  EXPECT_FALSE(DoReact(MakeUnit('b'), MakeUnit('a')));
  EXPECT_FALSE(DoReact(MakeUnit('B'), MakeUnit('a')));
  EXPECT_FALSE(DoReact(MakeUnit('b'), MakeUnit('A')));
  EXPECT_FALSE(DoReact(MakeUnit('B'), MakeUnit('A')));
}

TEST(ReactionChain, BuildsFromString) {
  EXPECT_EQ(ReactionChain(), BuildChain(""));

  const ReactionChain expected = {MakeUnit('a'), MakeUnit('A'), MakeUnit('b'),
                                  MakeUnit('B')};
  EXPECT_EQ(expected, BuildChain("aAbB"));
}

TEST(ResolveChain, WorksForEmptyChain) {
  const ReactionChain chain = {};
  EXPECT_EQ(chain, ResolveChain(chain));
}

TEST(ResolveChain, WorksForSingleElement) {
  const ReactionChain chain = BuildChain("a");
  EXPECT_EQ(chain, ResolveChain(chain));
}

TEST(ResolveChain, WorksForTwoReactingElements) {
  const ReactionChain input = BuildChain("aA");
  const ReactionChain expected = {};
  EXPECT_EQ(expected, ResolveChain(input));
}

TEST(ResolveChain, WorksForTwoGroupsOfReactingElementsInSequence) {
  const ReactionChain input = BuildChain("aAbB");
  const ReactionChain expected = {};
  EXPECT_EQ(expected, ResolveChain(input));
}

TEST(ResolveChain, WorksForTwoGroupsOfReactingElementsWithOverlap) {
  const ReactionChain input = BuildChain("aBbA");
  const ReactionChain expected = {};
  EXPECT_EQ(expected, ResolveChain(input));
}

TEST(ResolveChain, WorksForTwoGroupsOfWithoutReaction) {
  const ReactionChain chain = BuildChain("aBAb");
  EXPECT_EQ(chain, ResolveChain(chain));
}

TEST(ResolveChain, ExampleWorks) {
  const ReactionChain input = BuildChain("dabAcCaCBAcCcaDA");
  const ReactionChain expected = BuildChain("dabCBAcaDA");
  EXPECT_EQ(expected, ResolveChain(input));
}

TEST(FindShortestVariationLength, ExampleWorks) {
  const ReactionChain input = BuildChain("dabAcCaCBAcCcaDA");
  EXPECT_EQ(4, FindShortestVariationLength(input));
}
