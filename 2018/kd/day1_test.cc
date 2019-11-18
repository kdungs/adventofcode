#include "kd/day1.h"

#include "gtest/gtest.h"

using kd::day1::CalculateFrequency;
using kd::day1::FindFirstDuplicateFrequency;

TEST(CalculateFrequency, WorksForFirstExample) {
  EXPECT_EQ(3, CalculateFrequency({1, 1, 1}));
}

TEST(CalculateFrequency, WorksForSecondExample) {
  EXPECT_EQ(0, CalculateFrequency({1, 1, -2}));
}

TEST(CalculateFrequency, WorksForThirdExample) {
  EXPECT_EQ(-6, CalculateFrequency({-1, -2, -3}));
}

TEST(FindFirstDuplicateFrequency, WorksForFirstExample) {
  EXPECT_EQ(0, FindFirstDuplicateFrequency({+1, -1}));
}

TEST(FindFirstDuplicateFrequency, WorksForSecondExample) {
  EXPECT_EQ(10, FindFirstDuplicateFrequency({3, 3, 4, -2, -4}));
}

TEST(FindFirstDuplicateFrequency, WorksForThirdExample) {
  EXPECT_EQ(5, FindFirstDuplicateFrequency({-6, 3, 8, 5, -6}));
}

TEST(FindFirstDuplicateFrequency, WorksForFourthExample) {
  EXPECT_EQ(14, FindFirstDuplicateFrequency({7, 7, -2, -7, -4}));
}
