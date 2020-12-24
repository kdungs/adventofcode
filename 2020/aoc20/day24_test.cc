#include "aoc20/day24.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

using aoc20::day24::HexDir;
using aoc20::day24::HexPath;
using aoc20::day24::Part1;
using aoc20::day24::Part2;
using ::testing::ElementsAre;

constexpr char kExampleInput[] = R"(sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew
)";

std::vector<HexPath> ReadExamplePaths() {
  std::vector<HexPath> paths;
  HexPath path;
  std::stringstream ss{kExampleInput};
  while (ss >> path) {
    paths.push_back(path);
    path.clear();
  }
  return paths;
}

TEST(Parsing, WorksForExamplee) {
  auto paths = ReadExamplePaths();
  EXPECT_EQ(20, paths.size());
  EXPECT_THAT(
      paths[0],
      ElementsAre(HexDir::SouthEast, HexDir::SouthEast, HexDir::NorthWest,
                  HexDir::NorthEast, HexDir::NorthEast, HexDir::NorthEast,
                  HexDir::West, HexDir::SouthEast, HexDir::East,
                  HexDir::SouthWest, HexDir::West, HexDir::SouthWest,
                  HexDir::SouthWest, HexDir::West, HexDir::NorthEast,
                  HexDir::NorthEast, HexDir::West, HexDir::SouthEast,
                  HexDir::West, HexDir::SouthWest));
  EXPECT_THAT(
      paths[19],
      ElementsAre(HexDir::West, HexDir::SouthEast, HexDir::West, HexDir::East,
                  HexDir::East, HexDir::East, HexDir::NorthWest,
                  HexDir::NorthEast, HexDir::SouthEast, HexDir::NorthWest,
                  HexDir::West, HexDir::West, HexDir::SouthWest,
                  HexDir::NorthEast, HexDir::West));
}

TEST(Part1, WorksForExamples) { EXPECT_EQ(10, Part1(ReadExamplePaths())); }

TEST(Part2, WorksForExamples) { EXPECT_EQ(2208, Part2(ReadExamplePaths())); }
