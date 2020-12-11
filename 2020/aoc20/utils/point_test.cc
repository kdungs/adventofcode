#include "aoc20/utils/point.h"

#include <unordered_set>

#include "gtest/gtest.h"

using aoc20::utils::Point;

TEST(Bitset, CanBeUsedInSet) {
  using IntPoint = Point<int>;
  using IntPointSet = std::unordered_set<IntPoint, aoc20::utils::PointHash<int>>;


  IntPointSet s;
  s.insert(IntPoint{1, 2});
  s.insert(IntPoint{1, 3});
  EXPECT_EQ(2, s.size());
  EXPECT_EQ(1, s.count(IntPoint{1, 2}));
  EXPECT_EQ(1, s.count(IntPoint{1, 3}));
}
