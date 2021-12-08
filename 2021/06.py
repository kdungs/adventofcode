#!/usr/bin/env python

from collections import Counter, defaultdict

with open("data/06.txt") as f:
    data = f.read()

raw = map(int, data.split(","))
fish = Counter(raw)


def adv(fs):
    nxt = defaultdict(int)
    for age, cnt in fs.items():
        if age == 0:
            nxt[6] += cnt
            nxt[8] += cnt
        else:
            nxt[age - 1] += cnt
    return nxt


def advn(fs, n):
    for _ in range(n):
        fs = adv(fs)
    return fs


def cnt(fs):
    return sum(cnt for cnt in fs.values())


# Part 1
res = advn(fish, 80)
print(cnt(res))

# Part 2
res = advn(fish, 256)
print(cnt(res))
