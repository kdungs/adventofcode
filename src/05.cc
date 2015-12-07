#include <iostream>
#include <vector>
#include <string>

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
  auto nice_words = words
                  | view::remove_if(not_(has_n_vowels(3)))
                  | view::remove_if(not_(has_repeated_letter()))
                  | view::remove_if(has_substring("ab"))
                  | view::remove_if(has_substring("cd"))
                  | view::remove_if(has_substring("pq"))
                  | view::remove_if(has_substring("xy"));

  std::cout << distance(nice_words) << '\n';
}
