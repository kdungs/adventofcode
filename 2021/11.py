#!/usr/bin/env python3

import copy
import itertools as it

with open("data/11.txt") as f:
    lines = f.readlines()

energies = [[int(x) for x in line.strip()] for line in lines]


class Board:
    def __init__(self, energies):
        self.energies = copy.deepcopy(energies)  # ...
        self.rows = len(energies)
        self.cols = len(energies[0])
        self.num_flashes = 0

    def neighbours(self, row, col):
        ns = [
            (row - 1, col - 1),
            (row - 1, col),
            (row - 1, col + 1),
            (row, col - 1),
            (row, col + 1),
            (row + 1, col - 1),
            (row + 1, col),
            (row + 1, col + 1),
        ]
        return [
            (r, c)
            for r, c in ns
            if (r >= 0 and r < self.rows and c >= 0 and c < self.cols)
        ]

    def advance(self):
        flashed = [[False for _ in range(self.cols)] for _ in range(self.rows)]
        # Queue would feel more natural but stack is easier in py
        stack = list(it.product(range(self.rows), range(self.cols)))
        while len(stack) > 0:
            row, col = stack.pop()
            self.energies[row][col] += 1
            if flashed[row][col] or self.energies[row][col] < 10:
                continue
            flashed[row][col] = True
            self.num_flashes += 1
            stack += self.neighbours(row, col)
        for row in range(self.rows):
            for col in range(self.cols):
                if flashed[row][col]:
                    self.energies[row][col] = 0


board = Board(energies)
for _ in range(100):
    board.advance()
print(board.num_flashes)


board = Board(energies)
n = 0
while True:
    n += 1
    board.num_flashes = 0
    board.advance()
    if board.num_flashes == (board.rows * board.cols):
        break
print(n)
