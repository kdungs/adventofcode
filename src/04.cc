#include <algorithm>
#include <array>
#include <cassert>
#include <iomanip>
#include <iostream>
#include <iterator>

#include <openssl/md5.h>
#include <range/v3/all.hpp>
using namespace ranges;

#include "helpers.h"

template <std::size_t N = 16>
struct Digest {
  std::array<unsigned char, N> data;
};

auto hash_md5(const std::vector<unsigned char>& data) {
  auto digest = Digest<>{};
  MD5(data.data(), data.size(), &digest.data.front());
  return digest;
}

auto number_to_vec(unsigned int n) {
  auto res = std::vector<unsigned char>{};
  while (n > 0) {
    res.push_back('0' + n % 10);
    n /= 10;
  }
  std::reverse(std::begin(res), std::end(res));
  return std::move(res);
}

template <typename PRED>
auto find_index(const std::vector<unsigned char>& data, PRED&& p) {
  auto result = view::iota(1) | view::drop_while([&data, &p](auto n) {
                  auto v = number_to_vec(n);
                  auto c = view::concat(data, v) | to_vector;
                  auto h = hash_md5(c);
                  return !std::forward<PRED>(p)(h.data);
                });

  return *begin(result);
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  auto data = helpers::read_file<std::vector<unsigned char>>(argv[1]);
  auto i_5zeroes = find_index(data, [](const auto& digest) {
    return digest[0] == 0 && digest[1] == 0 && digest[2] < 16;
  });
  auto i_6zeroes = find_index(data, [](const auto& digest) {
    return all_of(digest | view::take(3), [](auto x) { return x == 0; });
  });

  std::cout << "5 zeroes: " << i_5zeroes << '\n';
  std::cout << "6 zeroes: " << i_6zeroes << '\n';
}
