#!/usr/bin/env python3

from collections import Counter, defaultdict
from typing import List


def is_small(cave: str) -> bool:
    return cave.islower()

def is_large(cave: str) -> bool:
    return cave.isupper()


class Caves:
    def __init__(self, lines: List[str]):
        self._adj = defaultdict(list)
        for line in lines:
            a, b = line.strip().split("-")
            self._adj[a].append(b)
            self._adj[b].append(a)

    def _can_visit_cave(self, path: List[str], cave: str) -> bool:
        return is_large(cave) or (cave not in path)

    def all_next_paths(self, path: List[str]) -> List[List[str]]:
        next_paths = []
        last = path[-1]
        for n in self._adj[last]:
            if not self._can_visit_cave(path, n):
                continue
            next_paths.append(path + [n])
        return next_paths

    def count_all_paths(self) -> int:
        paths = [["start"]]
        npaths = 0
        while len(paths) > 0:
            next_paths = []
            for path in paths:
                for npath in self.all_next_paths(path):
                    if npath[-1] == "end":
                        npaths += 1
                    else:
                        next_paths.append(npath)
            paths = next_paths
        return npaths


class Caves2(Caves):
    def _can_visit_cave(self, path: List[str], cave: str) -> bool:
        if is_large(cave):
            return True
        if cave == "start":
            return False
        cnts = Counter([p for p in path if is_small(p)])
        has_seen_small_cave_twice = any(c > 1 for k, c in cnts.items())
        limit = 2 - int(has_seen_small_cave_twice)
        return cnts.get(cave, 0) < limit


if __name__ == "__main__":
    with open("data/12.txt") as f:
        lines = f.readlines()
    caves = Caves(lines)
    print(caves.count_all_paths())
    caves2 = Caves2(lines)
    print(caves2.count_all_paths())
