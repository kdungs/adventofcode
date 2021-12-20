#!/usr/bin/env python3

from __future__ import annotations
from dataclasses import dataclass
from typing import List, Optional, Union
import itertools as it
import json


@dataclass
class SnailfishNumber:
    value: Optional[int]
    left: Optional[SailfishNumber]
    right: Optional[SailfishNumber]
    parent: Optional[SailfishNumber]

    def __repr__(self) -> str:
        if self.value is not None:
            return f"{self.value}"
        return f"[{self.left},{self.right}]"

    def _leftmost(self) -> SnailfishNumber:
        if self.value is not None:
            return self
        # Cannot be None by definition.
        return self.left._leftmost()

    def _rightmost(self) -> SnailfishNumber:
        if self.value is not None:
            return self
        # Cannot be None by definition.
        return self.right._rightmost()

    def _left_neighbour(self) -> Optional[SnailfishNumber]:
        # Have to go up as long as we are in the left subtree until we know
        # that there is nothing to the left or until we have a right sub-tree
        # to traverse into.
        if self.parent is None:
            return None
        if self is self.parent.right:
            return self.parent.left._rightmost()
        return self.parent._left_neighbour()

    def _right_neighbour(self) -> Optional[SnailfishNumber]:
        # Have to up as long as we are in the right subtree until we know that
        # there is nothing to the right or until we have a left sub-tree to
        # traverse into.
        if self.parent is None:
            return None
        if self is self.parent.left:
            return self.parent.right._leftmost()
        return self.parent._right_neighbour()

    def _explode(self, depth: int) -> bool:
        if self.value is not None:
            return False
        # By definition, there can never be a larger depth because as soon as
        # we'd create a number with a larger depth, we'd explode again.
        if depth < 4:
            if self.left._explode(depth + 1):
                return True
            return self.right._explode(depth + 1)
        # Too deep; explode! By definition, we are now at a node element with
        # two leaf element children.
        assert self.left is not None and self.left.value is not None
        assert self.right is not None and self.right.value is not None
        ln = self._left_neighbour()
        if ln is not None:
            ln.value += self.left.value
        rn = self._right_neighbour()
        if rn is not None:
            rn.value += self.right.value
        self.left.parent = None
        self.left = None
        self.right.parent = None
        self.right = None
        self.value = 0
        return True

    def _split(self) -> bool:
        if self.value is None:
            if self.left._split():
                return True
            return self.right._split()
        if self.value < 10:
            return False
        self.left = SnailfishNumber(self.value // 2, None, None, self)
        self.right = SnailfishNumber(
            self.value // 2 + self.value % 2,
            None,
            None,
            self,
        )
        self.value = None
        return True

    def reduce(self):
        while True:
            if self._explode(0):
                continue
            if not self._split():
                break

    def add(self, other: SnailfishNumber) -> SnailfishNumber:
        res = SnailfishNumber(None, self, other, None)
        self.parent = res
        other.parent = res
        res.reduce()
        return res

    def magnitude(self) -> int:
        if self.value is not None:
            return self.value
        return 3 * self.left.magnitude() + 2 * self.right.magnitude()


# Recursive type definition for whatever we get from parsing a line as JSON.
# ¯\_(ツ)_/¯
SnailfishNumberRepresentation = Union[int, List['SnRepr']]


def build_snailfish_number(
    representation: SnailfishNumberRepresentation,
) -> SnailfishNumber:
    if isinstance(representation, int):
        return SnailfishNumber(representation, None, None, None)
    l = build_snailfish_number(representation[0])
    r = build_snailfish_number(representation[1])
    n = SnailfishNumber(None, l, r, None)
    l.parent = n
    r.parent = n
    return n


def parse(line: str) -> SnailfishNumber:
    return build_snailfish_number(json.loads(line))


if __name__ == "__main__":
    with open("data/18.txt") as f:
        lines = f.readlines()
    # Part 1
    sns = [parse(line) for line in lines]
    sn = sns[0]
    for osn in sns[1:]:
        sn = sn.add(osn)
    print(sn.magnitude())
    # Part 2
    maxmag = 0
    for ll, rl in it.permutations(lines, 2):
        l = parse(ll)
        r = parse(rl)
        m = l.add(r).magnitude()
        if m > maxmag:
            maxmag = m
    print(maxmag)
