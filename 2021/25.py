#!/usr/bin/env python3

from __future__ import annotations
import enum
from typing import Dict, NamedTuple, Optional, Tuple


class Board(NamedTuple):
    rows: int
    cols: int
    east: Set[Tuple[int, int]]
    south: Set[Tuple[int, int]]

    def _coords(self, row: int, col: int) -> Tuple[int, int]:
        return (
            row % self.rows,
            col % self.cols,
        )

    def nextgen(self) -> Board:
        neast = set()
        nsouth = set()
        # First, east facing.
        for (row, col) in self.east:
            nxt = self._coords(row, col + 1)
            if nxt in self.east or nxt in self.south:
                neast.add((row, col))
            else:
                neast.add(nxt)
        # Then, south facing
        for (row, col) in self.south:
            nxt = self._coords(row + 1, col)
            if nxt in self.south or nxt in neast:
                nsouth.add((row, col))
            else:
                nsouth.add(nxt)
        return Board(self.rows, self.cols, neast, nsouth)

    @classmethod
    def parse(cls, data: str) -> Board:
        lines = data.split("\n")[:-1]
        east = set()
        south = set()
        for row, line in enumerate(lines):
            for col, char in enumerate(line):
                if char == ">":
                    east.add((row, col))
                elif char == "v":
                    south.add((row, col))
        return cls(len(lines), len(lines[0]), east, south)

    def __repr__(self) -> str:
        res = ""
        for row in range(self.rows):
            for col in range(self.cols):
                if (row, col) in self.east:
                    res += ">"
                elif (row, col) in self.south:
                    res += "v"
                else:
                    res += "."
            res += "\n"
        return res




with open("data/25.txt") as f:
    data = f.read()

board = Board.parse(data)
nxt = board.nextgen()
i = 1
while board != nxt:
    board = nxt
    nxt = board.nextgen()
    i += 1
print(i)
