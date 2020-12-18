#ifndef AOC20_DAY18_H_
#define AOC20_DAY18_H_

#include <iostream>
#include <numeric>
#include <stack>
#include <string>
#include <tuple>
#include <vector>

#include "absl/strings/str_cat.h"

namespace aoc20 {
namespace day18 {

using Num = int64_t;

enum class TokenType { Empty, Plus, Mul, ParenOpen, ParenClose, Number };

struct Token {
  TokenType type;
  Num value;
};

TokenType ParseTokenType(char t) {
  if (t == '+') {
    return TokenType::Plus;
  }
  if (t == '*') {
    return TokenType::Mul;
  }
  if (t == '(') {
    return TokenType::ParenOpen;
  }
  if (t == ')') {
    return TokenType::ParenClose;
  }
  if (t >= '0' && t <= '9') {
    return TokenType::Number;
  }
  return TokenType::Empty;
}

std::vector<Token> ParseTokens(const std::string& line) {
  std::vector<Token> toks;
  for (char t : line) {
    TokenType tt = ParseTokenType(t);
    if (tt == TokenType::Empty) {
      continue;
    }
    Token tok{tt, 0};
    if (tt == TokenType::Number) {
      tok.value = t - '0';
    }
    toks.push_back(tok);
  }
  return toks;
}

std::tuple<Num, std::size_t> EvalSub(const std::vector<Token>& tokens,
                                     std::size_t position) {
  if (position >= tokens.size()) {
    throw std::runtime_error(
        absl::StrCat("cannot evaluate sub expression at ", position));
  }
  const auto ReadNum = [&tokens, &position]() {
    const Token& tok = tokens[position];
    if (tok.type == TokenType::Number) {
      ++position;
      return tok.value;
    }
    if (tok.type == TokenType::ParenOpen) {
      auto [v, p] = EvalSub(tokens, position + 1);
      position = p;
      return v;
    }
    throw std::runtime_error("expected number or sub-expression");
  };

  // Assume () are not allowed.
  Num lhs = ReadNum();
  while (position < tokens.size() &&
         tokens[position].type != TokenType::ParenClose) {
    const Token& cur = tokens[position];  // either * or +
    ++position;
    Num rhs = ReadNum();
    if (cur.type == TokenType::Plus) {
      lhs += rhs;
    } else if (cur.type == TokenType::Mul) {
      lhs *= rhs;
    } else {
      throw std::runtime_error("expected operator");
    }
  }
  return {lhs, position + 1};
}

Num Eval(const std::string& line) {
  std::vector<Token> toks = ParseTokens(line);
  auto [res, pos] = EvalSub(toks, 0);
  return res;
}

int64_t Part1(const std::vector<std::string>& lines) {
  return std::accumulate(
      lines.begin(), lines.end(), 0ll,
      [](int64_t acc, const std::string& line) { return acc + Eval(line); });
}

std::tuple<Num, std::size_t> EvalSub2(const std::vector<Token>& tokens,
                                      std::size_t position) {
  if (position >= tokens.size()) {
    throw std::runtime_error(
        absl::StrCat("cannot evaluate sub expression at ", position));
  }
  const auto ReadNum = [&tokens, &position]() {
    const Token& tok = tokens[position];
    if (tok.type == TokenType::Number) {
      ++position;
      return tok.value;
    }
    if (tok.type == TokenType::ParenOpen) {
      auto [v, p] = EvalSub2(tokens, position + 1);
      position = p;
      return v;
    }
    throw std::runtime_error("expected number or sub-expression");
  };

  // Assume () are not allowed.
  std::stack<Num> nums;
  nums.push(ReadNum());
  while (position < tokens.size() &&
         tokens[position].type != TokenType::ParenClose) {
    const Token& cur = tokens[position];  // either * or +
    ++position;
    Num rhs = ReadNum();
    if (cur.type == TokenType::Plus) {
      Num lhs = nums.top();
      nums.pop();
      nums.push(lhs + rhs);
    } else if (cur.type == TokenType::Mul) {
      nums.push(rhs);
    } else {
      throw std::runtime_error("expected operator");
    }
  }

  Num total{nums.top()};
  nums.pop();
  while (!nums.empty()) {
    total *= nums.top();
    nums.pop();
  }
  return {total, position + 1};
}

Num Eval2(const std::string& line) {
  std::vector<Token> toks = ParseTokens(line);
  auto [res, pos] = EvalSub2(toks, 0);
  return res;
}

int64_t Part2(const std::vector<std::string>& lines) {
  return std::accumulate(
      lines.begin(), lines.end(), 0ll,
      [](int64_t acc, const std::string& line) { return acc + Eval2(line); });
}

}  // namespace day18
}  // namespace aoc20

#endif  // AOC20_DAY18_H_
