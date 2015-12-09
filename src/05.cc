#include <iostream>
#include <vector>
#include <string>
#include <type_traits>

#include <boost/algorithm/string/predicate.hpp>
#include <range/v3/all.hpp>

#include "helpers.h"

using namespace ranges;

auto has_n_vowels(std::size_t n) {
  return [n](auto&& rng) {
    using Rng = decltype((rng));
    return count_if(std::forward<Rng>(rng), [](auto c) {
      return c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u';
    }) >= n;
  };
}

auto has_repeated_letter() {
  return [](auto&& rng) {
    using Rng = decltype((rng));
    return any_of(view::zip_with([](auto lhs, auto rhs) { return lhs == rhs; },
                                 std::forward<Rng>(rng),
                                 std::forward<Rng>(rng) | view::drop(1)),
                  [](bool x) { return x; });
  };
}

auto has_substring(std::string s) {
  return [s=std::move(s)](auto&& rng) {
    using Rng = decltype((rng));
    return boost::contains(std::forward<Rng>(rng) | to_vector, s);
  };
}

auto has_interspersed_pair() {
  return [](auto&& rng) {
    using Rng = decltype((rng));
    auto triplets = view::zip(std::forward<Rng>(rng),
                              std::forward<Rng>(rng) | view::drop(1),
                              std::forward<Rng>(rng) | view::drop(2));
    return any_of(triplets, [](auto triplet) {
      return std::get<0>(triplet) == std::get<2>(triplet);
    });
  };
}

template <typename T>
struct value_pair : std::tuple<T, T> {
  value_pair(T f, T s) : std::tuple<T, T>{std::move(f), std::move(s)} {}
};

template <typename T, typename U=typename std::decay_t<T>>
auto make_value_pair(T first, T second) {
  return value_pair<U>{std::move(first), std::move(second)};
};

template <typename T>
std::ostream& operator<<(std::ostream& os, const value_pair<T>& p) {
  return (os << '(' << std::get<0>(p) << ',' << std::get<1>(p) << ')');
}

auto has_repeating_pairs() {
  return [](auto&& rng) {
    using Rng = decltype((rng));

    auto make_value_pair_ = [](auto lhs, auto rhs) {
      return make_value_pair(lhs, rhs);
    };
    auto equal_ = [](auto lhs, auto rhs) { return lhs == rhs; };
    auto drop_half = [](auto&& rng) {
      using Rng = decltype((rng));
      return std::forward<Rng>(rng)
           | view::drop(distance(std::forward<Rng>(rng)) / 2);
    };

    auto pairs = view::zip_with(make_value_pair_,
                                std::forward<Rng>(rng),
                                std::forward<Rng>(rng) | view::drop(1))
               | view::group_by(equal_)
               | view::transform(drop_half)
               | view::join
               | to_vector;
    pairs |= action::sort;
    return (distance(pairs) != distance(pairs | view::unique));
  };
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }

  auto not_ = [](auto&& f) {
    return [f=std::forward<decltype((f))>(f)](auto&&... args) {
      return !f(std::forward<decltype((args))>(args)...);
    };
  };

  auto data = helpers::read_file<std::string>(argv[1]);
  auto words = data | view::split('\n');
  auto nice_words_1 = words
                  | view::remove_if(not_(has_n_vowels(3)))
                  | view::remove_if(not_(has_repeated_letter()))
                  | view::remove_if(has_substring("ab"))
                  | view::remove_if(has_substring("cd"))
                  | view::remove_if(has_substring("pq"))
                  | view::remove_if(has_substring("xy"));

  auto nice_words_2 = words
                    | view::remove_if(not_(has_repeating_pairs()))
                    | view::remove_if(not_(has_interspersed_pair()));
  
  std::cout << distance(nice_words_1) << '\n';
  std::cout << distance(nice_words_2) << '\n';
}
