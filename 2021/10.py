#!/usr/bin/env python3

import statistics

SCORES1 = {
    ")": 3,
    "]": 57,
    "}": 1197,
    ">": 25137,
}

SCORES2 = {
    ")": 1,
    "]": 2,
    "}": 3,
    ">": 4,
}

MATCHING = {
    "(": ")",
    "[": "]",
    "{": "}",
    "<": ">",
}

OPEN = MATCHING.keys()
CLOSE = MATCHING.values()

with open("data/10.txt") as f:
    lines = f.readlines()

score1 = 0
scores2 = []
for line in lines:
    line = line.strip()
    stack = []
    broken = False
    for c in line:
        if c in OPEN:
            stack.append(c)
        elif c in CLOSE:
            o = stack.pop()
            if not MATCHING[o] == c:
                score1 += SCORES1[c]
                broken = True
                break
    # For part 2, rewind stack and add score missing closing braces.
    if not broken:
        score2 = 0
        for missing in reversed(stack):
            score2 *= 5
            score2 += SCORES2[MATCHING[missing]]
        scores2.append(score2)
print(score1)
print(int(statistics.median(scores2)))
