#!/usr/bin/env python3

import statistics

with open("data/07.txt") as f:
    crabs = list(map(int, f.read().split(",")))

# Part 1
def fuel(crabs, x):
    return sum(abs(x - c) for c in crabs)

median = int(statistics.median(crabs))
print(fuel(crabs, median))


# Part 2
# Solution is quadratic... Can we do better?

def fuel2(crabs, x):
    total = 0
    for crab in crabs:
        delta = abs(x - crab)
        total += delta * (delta + 1) / 2
    return int(total)

minfuel = fuel2(crabs, min(crabs))
for x in range(min(crabs) + 1, max(crabs)):
    f = fuel2(crabs, x)
    if f < minfuel:
        minfuel = f
print(minfuel)
