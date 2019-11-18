#include "kd/day2.h"

#include <string>
#include <vector>

#include "gtest/gtest.h"

using kd::day2::Checksum;
using kd::day2::CountLetters;
using kd::day2::FindCommonLettersOfCorrectBoxIds;
using kd::day2::HammingDistance;
using kd::day2::HasLetterNTimes;
using kd::day2::LetterCounts;

TEST(CountLetters, WorksForEmptyString) {
  const std::string s = "";
  EXPECT_EQ(LetterCounts{}, CountLetters(s));
}

TEST(CountLetters, WorksForSingleCharacter) {
  const std::string s = "a";
  EXPECT_EQ(LetterCounts({{'a', 1}}), CountLetters(s));
}

TEST(CountLetters, WorksForStringOfUniqueCharacters) {
  const std::string s = "abcdefg";
  EXPECT_EQ(LetterCounts({{'a', 1},
                          {'b', 1},
                          {'c', 1},
                          {'d', 1},
                          {'e', 1},
                          {'f', 1},
                          {'g', 1}}),
            CountLetters(s));
}

TEST(CountLetters, WorksForStringOfRepeatedCharacter) {
  const std::string s = "aaaaaaa";
  EXPECT_EQ(LetterCounts({{'a', 7}}), CountLetters(s));
}

TEST(CountLetters, WorksForMixedString) {
  const std::string s = "adac";
  EXPECT_EQ(LetterCounts({{'a', 2}, {'c', 1}, {'d', 1}}), CountLetters(s));
}

TEST(HasNumberOfLetters, Example1) {
  constexpr char kExample1[] = "abcdef";
  const LetterCounts counts = CountLetters(kExample1);
  EXPECT_FALSE(HasLetterNTimes(counts, 2));
  EXPECT_FALSE(HasLetterNTimes(counts, 3));
}

TEST(HasNumberOfLetters, Example2) {
  constexpr char kExample2[] = "bababc";
  const LetterCounts counts = CountLetters(kExample2);
  EXPECT_TRUE(HasLetterNTimes(counts, 2));
  EXPECT_TRUE(HasLetterNTimes(counts, 3));
}

TEST(HasNumberOfLetters, Example3) {
  constexpr char kExample3[] = "abbcde";
  const LetterCounts counts = CountLetters(kExample3);
  EXPECT_TRUE(HasLetterNTimes(counts, 2));
  EXPECT_FALSE(HasLetterNTimes(counts, 3));
}

TEST(HasNumberOfLetters, Example4) {
  constexpr char kExample4[] = "abcccd";
  const LetterCounts counts = CountLetters(kExample4);
  EXPECT_FALSE(HasLetterNTimes(counts, 2));
  EXPECT_TRUE(HasLetterNTimes(counts, 3));
}

TEST(HasNumberOfLetters, Example5) {
  constexpr char kExample5[] = "aabcdd";
  const LetterCounts counts = CountLetters(kExample5);
  EXPECT_TRUE(HasLetterNTimes(counts, 2));
  EXPECT_FALSE(HasLetterNTimes(counts, 3));
}

TEST(HasNumberOfLetters, Example6) {
  constexpr char kExample6[] = "abcdee";
  const LetterCounts counts = CountLetters(kExample6);
  EXPECT_TRUE(HasLetterNTimes(counts, 2));
  EXPECT_FALSE(HasLetterNTimes(counts, 3));
}

TEST(HasNumberOfLetters, Example7) {
  constexpr char kExample7[] = "ababab";
  const LetterCounts counts = CountLetters(kExample7);
  EXPECT_FALSE(HasLetterNTimes(counts, 2));
  EXPECT_TRUE(HasLetterNTimes(counts, 3));
}

TEST(Checksum, WorksForExample) {
  const std::vector<std::string> box_ids{
      "abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab",
  };

  EXPECT_EQ(12, Checksum(box_ids));
}

TEST(HammingDistance, WorksForExample1) {
  constexpr char kLhs[] = "abcde";
  constexpr char kRhs[] = "axcye";
  EXPECT_EQ(2, HammingDistance(kLhs, kRhs));
}

TEST(HammingDistance, WorksForExample2) {
  constexpr char kLhs[] = "fghij";
  constexpr char kRhs[] = "fguij";
  EXPECT_EQ(1, HammingDistance(kLhs, kRhs));
}

TEST(FindCommonLettersOfCorrectBoxIds, WorksForExample) {
  const std::vector<std::string> box_ids{"abcde", "fghij", "klmno", "pqrst",
                                         "fguij", "axcye", "wvxyz"};
  EXPECT_EQ("fgij", FindCommonLettersOfCorrectBoxIds(box_ids));
}
