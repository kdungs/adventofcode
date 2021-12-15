#!/usr/bin/env python3

# This is SSSP -> Dijkstra

import heapq
from typing import List, Tuple

INF = float("inf")


with open("data/15.txt") as f:
    lines = f.readlines()

risks = [
    [int(x) for x in line.strip()]
    for line in lines
]


class Cave:
    def __init__(self, risks: List[List[int]]):
        self._risks = risks
        self._rows = len(risks)
        self._cols = len(risks[0])

    def _neighbours(self, row: int, col: int) -> List[Tuple[int, int]]:
        ns = [
            (row - 1, col),
            (row, col - 1),
            (row, col + 1),
            (row + 1, col),
        ]
        return [
            (nr, nc)
            for nr, nc in ns
            if nr >= 0 and nr < self._rows and nc >= 0 and nc < self._cols
        ]

    def find_min_risk_path(self) -> int:
        start = (0, 0)
        end = (self._rows - 1, self._cols - 1)
        dists = [[INF for _ in range(self._cols)] for _ in range(self._rows)]
        dists[0][0] = 0
        heap = [(0, start)]
        while len(heap) > 0:
            _, (r, c) = heapq.heappop(heap)
            if (r, c) == end:
                break
            dist = dists[r][c]
            for nr, nc in self._neighbours(r, c):
                alt = dist + self._risks[nr][nc]
                if alt < dists[nr][nc]:
                    dists[nr][nc] = alt
                    heapq.heappush(heap, (alt, (nr, nc)))
        return dists[end[0]][end[1]]


# Part 1
print(Cave(risks).find_min_risk_path())


# Part 2
rows = len(risks)
cols = len(risks[0])
risks2 = [[0 for _ in range(cols * 5)] for _ in range(5 * rows)]
for rg in range(5):
    for cg in range(5):
        for r in range(rows):
            row = rg * rows + r
            for c in range(cols):
                col = cg * cols + c
                risk = risks[r][c] + rg + cg
                while risk > 9:
                    risk -= 9
                risks2[row][col] = risk
print(Cave(risks2).find_min_risk_path())
