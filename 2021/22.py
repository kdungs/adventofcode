#!/usr/bin/env python3

from __future__ import annotations
import itertools as it
from typing import List, NamedTuple

with open("data/22.txt") as f:
    lines = f.readlines()

def parse(line):
    mode, coords = line.split(" ")
    xstr, ystr, zstr = coords.split(",")
    return mode, tuple(
        [int(x) for x in s[2:].split("..")]
        for s in coords.split(",")
    )

# Part 1
# def start_range(l, r):
#     return range(max(l, -50), min(r, 50) + 1)
#
# on = set()
# for line in lines:
#     mode, (xs, ys, zs) = parse(line)
#     coords = set(it.product(start_range(*xs), start_range(*ys), start_range(*zs)))
#     if mode == "on":
#        on |= coords
#     else:
#         on -= coords
#
# print(len(on))


# Part 2 would be a bit too memory hungry.
class Range(NamedTuple):
    """ Half-open so we can easily determine size.
        Manually ensure left <= right.
    """
    left: int
    right: int

    def __len__(self) -> int:
        return self.right - self.left

    def overlap(self, other: Range) -> Range:
        l = max(self.left, other.left)
        r = min(self.right, other.right)
        if l > r:
            return Range(0, 0)
        return Range(l, r)

assert len(Range(0, 10)) == 10
assert len(Range(1, 10)) == 9
assert Range(0, 100).overlap(Range(0, 100)) == Range(0, 100)
assert Range(1, 100).overlap(Range(0, 100)) == Range(1, 100)
assert Range(0, 100).overlap(Range(0, 99)) == Range(0, 99)


class Cuboid(NamedTuple):
    x: Range
    y: Range
    z: Range

    def union(self, other: Cuboid) -> List[Cuboid]:
        xo = self.x.overlap(other.x)
        if len(xo) == 0:
            return [self, other]
        yo = self.y.overlap(other.y)
        if len(yo) == 0:
            return [self, other]
        zo = self.z.overlap(other.z)
        if len(zo) == 0:
            return [self, other]
