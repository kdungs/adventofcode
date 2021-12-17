#!/usr/bin/env python3

from __future__ import annotations
from dataclasses import dataclass
from typing import List


def sign(x: int) -> int:
    if x == 0:
        return x
    if x > 0:
        return 1
    return -1


@dataclass
class Probe:
    x: int
    y: int
    dx: int
    dy: int

    @classmethod
    def create(cls, dx: int, dy: int) -> Probe:
        return cls(0, 0, dx, dy)

    def step(self) -> Probe:
        return Probe(
            self.x + self.dx,
            self.y + self.dy,
            self.dx - sign(self.dx),
            self.dy - 1,
        )


@dataclass
class TargetArea:
    minx: int
    maxx: int
    miny: int
    maxy: int

    def contains(self, x: int, y: int) -> bool:
        if x < self.minx:
            return False
        if x > self.maxx:
            return False
        if y < self.miny:
            return False
        if y > self.maxy:
            return False
        return True

    @classmethod
    def parse(cls, line: str) -> TargetArea:
        l, r = line.strip().replace("target area: ", "").split(", ")
        minx, maxx = l.replace("x=", "").split("..")
        miny, maxy = r.replace("y=", "").split("..")
        return cls(int(minx), int(maxx), int(miny), int(maxy))


def trajectory_in_target_area(p: Probe, ta: TargetArea) -> bool:
    while True:
        p = p.step()
        if p.x < ta.minx and p.dx == 0:
            return False
        if p.x > ta.maxx:
            return False
        if p.y < ta.miny:
            return False
        if ta.contains(p.x, p.y):
            return True


def highest_y(p: Probe) -> int:
    nextp = p.step()
    while nextp.y > p.y:
        p = nextp
        nextp = nextp.step()
    return p.y


def part1(ta: TargetArea) -> int:
    best_probe = None
    best_dy = 0
    for dx in range(ta.minx):
        for dy in range(200):
            p = Probe.create(dx, dy)
            if trajectory_in_target_area(p, ta) and dy > best_dy:
                best_dy = dy
                best_probe = p
    return (highest_y(best_probe))


def part2(ta: TargetArea) -> int:
    good_probes = []
    for dx in range(2 * ta.maxx):
        for dy in range(ta.miny, 200):
            p = Probe.create(dx, dy)
            if trajectory_in_target_area(p, ta):
                good_probes.append(p)
    return len(good_probes)


if __name__ == "__main__":
    with open("data/17.txt") as f:
        line = f.read()
    ta = TargetArea.parse(line)
    print(part1(ta))
    print(part2(ta))
