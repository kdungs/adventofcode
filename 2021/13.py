#!/usr/bin/env python3

from typing import List, Tuple, Set
import functools as ft


Point = Tuple[int, int]


def parse_coord(line: str) -> Point:
    x, y = line.strip().split(",")
    return int(x), int(y)


def fold(coords: Set[Point], fold: str) -> Set[Point]:
    axis, val = fold[11:].split("=")
    val = int(val)
    if axis == "x":
        return {
            (x if x < val else 2 * val - x, y)
            for (x, y) in coords
            if x != val
        }
    return {
        (x, y if y < val else 2 * val - y)
        for (x, y) in coords
        if y != val
    }


def render(coords: Set[Point]) -> None:
    maxx = max(x for x, _ in coords)
    maxy = max(y for _, y in coords)
    for y in range(maxy + 1):
        print("".join(
            "█" if (x, y) in coords else " "
            for x in range(maxx + 1)
        ))


if __name__ == "__main__":
    with open("data/13.txt") as f:
        lines = f.read().strip()
    coord_lines, fold_lines = lines.split("\n\n")
    coords = {parse_coord(line) for line in coord_lines.split("\n")}
    folds = fold_lines.split("\n")
    # Part 1
    pt1 = fold(coords, folds[0])
    print(len(pt1))
    # Part 2
    result = ft.reduce(fold, folds, coords)
    render(result)
