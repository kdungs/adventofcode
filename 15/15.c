#include <stdint.h>
#include <stdio.h>

#define SCALING 2147483647
#define MASK 0xFFFF

typedef struct {
  uint64_t factor;
  uint64_t state;
} Generator;

void advance(Generator* g) {
  g->state = (g->factor * g->state) % SCALING;
}

int eval(uint64_t a, uint64_t b) {
  int16_t ma = MASK & a;
  int16_t mb = MASK & b;
  return ma == mb;
}

void advance_(int multiple, Generator* g) {
  do {
    advance(g);
  } while (g->state % multiple != 0);
}

int main() {
  Generator a = {16807, 873};
  Generator b = {48271, 583};
  int count = 0;
  for (uint64_t i = 0; i < 40000000ul; i++) {
    advance(&a);
    advance(&b);
    if (eval(a.state, b.state)) {
      count++;
    }
  }
  printf("%d\n", count);

  // Part 2
  Generator a_ = {16807, 873};
  Generator b_ = {48271, 583};
  int count_ = 0;
  for (uint64_t i = 0; i < 5000000ul; i++) {
    advance_(4, &a_);
    advance_(8, &b_);
    if (eval(a_.state, b_.state)) {
      count_++;
    }
  }
  printf("%d\n", count_);
}
