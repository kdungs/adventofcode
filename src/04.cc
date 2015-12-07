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

int main(int argc, char* argv[]) {
  if (argc != 2) {
    return EXIT_FAILURE;
  }
  auto data = helpers::read_file<std::vector<unsigned char>>(argv[1]);
  auto result = view::iota(1) | view::drop_while([&data](auto n) {
                  auto v = number_to_vec(n);
                  auto c = view::concat(data, v) | to_vector;
                  auto h = hash_md5(c);
                  return h.data[0] != 0 || h.data[1] != 0 || h.data[2] >= 16;
                });

  auto xs = *begin(result);
  std::cout << xs << '\n';
}
