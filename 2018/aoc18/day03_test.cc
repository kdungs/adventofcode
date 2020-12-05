#include "aoc18/day03.h"

#include "absl/types/optional.h"
#include "gtest/gtest.h"

using aoc18::day03::Grid;
using aoc18::day03::GridState;
using aoc18::day03::ParseRectangle;
using aoc18::day03::Rectangle;

TEST(Rectangle, ParsingFailsForEmptyString) {
  EXPECT_EQ(absl::nullopt, ParseRectangle(""));
}

TEST(Rectangle, ParsingFailsForInvalidString) {
  EXPECT_EQ(absl::nullopt, ParseRectangle("1 2 3 4 5 6"));
}

TEST(Rectangle, ParsesFromString) {
  const absl::optional<Rectangle> maybe_rect =
      ParseRectangle("#1 @ 2,3: 23x42");
  ASSERT_NE(absl::nullopt, maybe_rect);

  const Rectangle& rect = maybe_rect.value();
  EXPECT_EQ(1, rect.id());
  EXPECT_EQ(2, rect.x());
  EXPECT_EQ(3, rect.y());
  EXPECT_EQ(23, rect.w());
  EXPECT_EQ(42, rect.h());
}

TEST(GridState, AdditionWorks) {
  EXPECT_EQ(GridState::kEmpty, GridState::kEmpty + GridState::kEmpty);

  EXPECT_EQ(GridState::kOne, GridState::kEmpty + GridState::kOne);
  EXPECT_EQ(GridState::kOne, GridState::kOne + GridState::kEmpty);

  EXPECT_EQ(GridState::kMore, GridState::kEmpty + GridState::kMore);
  EXPECT_EQ(GridState::kMore, GridState::kOne + GridState::kOne);
  EXPECT_EQ(GridState::kMore, GridState::kOne + GridState::kMore);
  EXPECT_EQ(GridState::kMore, GridState::kMore + GridState::kEmpty);
  EXPECT_EQ(GridState::kMore, GridState::kMore + GridState::kOne);
  EXPECT_EQ(GridState::kMore, GridState::kMore + GridState::kMore);
}

TEST(Grid, EmptyGridHasZeroOverlapArea) {
  const Grid g(/*width=*/10, /*height=*/10);
  EXPECT_EQ(0, g.OverlapArea());
}

TEST(Grid, SingleRectangleDoesNotOverlap) {
  const Rectangle rect = Rectangle(/*id=*/0, /*x=*/0, /*y=*/0,
                                   /*w=*/10, /*h=*/10);
  const Grid g = Grid(/*width=*/10, /*height=*/10).AddRectangle(rect);
  EXPECT_EQ(0, g.OverlapArea());
  EXPECT_FALSE(g.DoesOverlap(rect));
}

TEST(Grid, SameRectangleDoesOverlap) {
  const Rectangle rect = Rectangle(/*id=*/0, /*x=*/0, /*y=*/0,
                                   /*w=*/10, /*h=*/10);
  const Grid g =
      Grid(/*width=*/10, /*height=*/10).AddRectangle(rect).AddRectangle(rect);
  EXPECT_EQ(100, g.OverlapArea());
  EXPECT_TRUE(g.DoesOverlap(rect));
}

TEST(Grid, BuildsFromVector) {
  const Grid g =
      Grid::Build({Rectangle(/*id=*/0, /*x=*/0, /*y=*/0, /*w=*/9, /*h=*/9),
                   Rectangle(/*id=*/1, /*x=*/1, /*y=*/0, /*w=*/9, /*h=*/9),
                   Rectangle(/*id=*/2, /*x=*/0, /*y=*/1, /*w=*/9, /*h=*/9),
                   Rectangle(/*id=*/3, /*x=*/1, /*y=*/1, /*w=*/9, /*h=*/9)});

  EXPECT_EQ(96, g.OverlapArea());
}

TEST(Grid, FindsNonOverlapping) {
  const Rectangle rect =
      Rectangle(/*id=*/23, /*x=*/10, /*y=*/10, /*w=*/90, /*h=*/999);
  const std::vector<Rectangle> rects = {
      Rectangle(/*id=*/0, /*x=*/0, /*y=*/0, /*w=*/9, /*h=*/9),
      Rectangle(/*id=*/1, /*x=*/1, /*y=*/0, /*w=*/9, /*h=*/9),
      Rectangle(/*id=*/2, /*x=*/0, /*y=*/1, /*w=*/9, /*h=*/9),
      Rectangle(/*id=*/3, /*x=*/1, /*y=*/1, /*w=*/9, /*h=*/9), rect};
  const Grid g = Grid::Build(rects);

  const absl::optional<Rectangle> maybe_nonoverlapping_rect =
      g.FindNonOverlapping(rects);

  ASSERT_NE(absl::nullopt, maybe_nonoverlapping_rect);

  const Rectangle& r = maybe_nonoverlapping_rect.value();
  EXPECT_EQ(23, r.id());
  EXPECT_EQ(10, r.x());
  EXPECT_EQ(10, r.y());
  EXPECT_EQ(90, r.w());
  EXPECT_EQ(999, r.h());
}
