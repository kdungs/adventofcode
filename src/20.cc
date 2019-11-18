#include <cmath>
#include <iostream>
#include <numeric>
#include <unordered_map>
#include <unordered_set>
#include <type_traits>

template <typename T=unsigned long>
struct Dividers {
  std::unordered_map<T, std::unordered_set<T>> dividers_map;

  const auto& dividers(T n) {
    auto ds = dividers_map.find(n);
    if (ds != std::end(dividers_map)) {
      return ds->second;
    }
    auto& ds_ = dividers_map[n];
    ds_.insert(n);
    ds_.insert(1);
    for (auto i = T{2}; i <= static_cast<T>(std::sqrt(n)); ++i) {
      if (n % i == 0) {
        ds_.insert(i);
        ds_.insert(n / i);
        for (auto d : dividers(i)) {
          ds_.insert(d);
        }
        for (auto d : dividers(n / i)) {
          ds_.insert(d);
        }
      };
    }
    return ds_;
  }
};

template <typename T=unsigned long>
struct Street {
  Dividers<T> ds;

  auto presentsAtHouse(T n) {
    auto ds_ = ds.dividers(n);
    return T{10} * std::accumulate(std::begin(ds_), std::end(ds_), T{0});
  }

  auto presentsAtHouse_2(T n) {
    auto ds_ = ds.dividers(n);
    return T{11} * std::accumulate(std::begin(ds_), std::end(ds_), T{0},
                                   [n](auto acc, auto d) {
                                     return acc + (n / d <= 50 ? d : 0);
                                   });
  }
};

int main() {
  auto s = Street<>{};
  auto i = 1ull;
  auto input = 33100000ull;
  while (s.presentsAtHouse(i) < input) { ++i; }
  std::cout << i << '\n';
  i = 1ull;
  while (s.presentsAtHouse_2(i) < input) { ++i; }
  std::cout << i << '\n';
}
