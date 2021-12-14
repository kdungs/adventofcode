#!/usr/bin/env python3

# If we formulate the production rules as (AB -> Ax, xB), the order of tuples
# is no longer relevant and we can solve the whole problem in terms of counters
# which deals with the O(2^n) memory usage.

from collections import Counter
from typing import Dict
import itertools as it

with open("data/14.txt") as f:
    lines = f.readlines()

polymer = Counter(f"{l}{r}" for l, r in it.pairwise(lines[0].strip()))
last = lines[0][-2]

rules = {}
for line in lines[2:]:
    l, r = line.strip().split(" -> ")
    rules[f"{l}"] = (f"{l[0]}{r}", f"{r}{l[1]}")
print(rules)

# Assume that the rules are comprehensive
for v in rules.values():
    assert v[0] in rules
    assert v[1] in rules


def nextgen(poly: Counter) -> Counter:
    ng = Counter()
    for k, cnt in poly.items():
        l, r = rules.get(k)
        ng[l] += cnt
        ng[r] += cnt
    return ng


def ngens(n: int, poly: Counter) -> Counter:
    for _ in range(n):
        poly = nextgen(poly)
    return poly


def result(poly: Counter, last: str) -> int:
    c = Counter([last])
    for k, cnt in poly.items():
        l, r = k
        c[l] += cnt
    return max(c.values()) - min(c.values())


# Part 1
print(result(ngens(10, polymer), last))

# Part 2
print(result(ngens(40, polymer), last))
