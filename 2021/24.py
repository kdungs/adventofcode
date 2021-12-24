#!/usr/bin/env python3

import itertools as it

with open("data/24.txt") as f:
    lines = [line.strip() for line in f.readlines()]

plen = 18
nump = len(lines) // plen

instructions = iter(lines)
programs = [list(it.islice(instructions, plen)) for _ in range(nump)]

# We only have a chance to reduce z if params[i][0] is 26;
# So we need to make sure that we can hit the correct value on those
params = [
    (
        int(p[4].split(" ")[-1]),
        int(p[5].split(" ")[-1]),
        int(p[15].split(" ")[-1]),
    )
    for p in programs
]

def p(w, z, a, b, c):
    div = z // a
    rest = z % 26
    if rest + b == w:
        return div
    return div * 26 + w + c


def build_solution(digits, z, gen):
    i = len(digits)
    if i == 14 and z == 0:
        return digits
    a, b, c = params[i]
    if a == 1:
        for w in gen():
            res = build_solution(
                digits + str(w),
                p(w, z, a, b, c),
                gen,
            )
            if res is not None:
                return res
    else:
        rest = z % 26
        for w in gen():
            if rest + b == w:
                res = build_solution(
                    digits + str(w),
                    p(w, z, a, b, c),
                    gen,
                )
                if res is not None:
                    return res
    return None


pt1 = build_solution("", 0, lambda: range(9, 0, -1))
print(pt1)

pt2 = build_solution("", 0, lambda: range(1, 10))
print(pt2)
