#include <benchmark/benchmark.h>

#include <iostream>
#include <string>
#include <vector>

#include "aoc20/day05.h"

template <typename FN>
struct BM {
  void operator()(benchmark::State& state,
                  const std::vector<std::string>& lines) const {
    for (auto s : state) {
      auto res = f(lines);
      benchmark::DoNotOptimize(res);
    }
  }

  const FN& f;
};

template <typename FN>
BM<FN> MakeBench(const FN& f) {
  return BM<FN>{f};
}

int main(int argc, char** argv) {
  // Prepare input
  std::vector<std::string> lines;
  std::string line;
  while (std::getline(std::cin, line)) {
    lines.push_back(line);
  }

  benchmark::RegisterBenchmark("Base version", MakeBench(aoc20::day05::Part2),
                               lines);
  benchmark::RegisterBenchmark("Version 2",
                               MakeBench(aoc20::day05::Part2Version2), lines);
  benchmark::RegisterBenchmark("Version 3",
                               MakeBench(aoc20::day05::Part2Version3), lines);
  benchmark::Initialize(&argc, argv);
  benchmark::RunSpecifiedBenchmarks();
}
