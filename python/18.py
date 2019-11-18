# Game of Life - yay!

import numpy as np
import sys
import itertools as it

def parse_input(filename):
    grid = np.zeros((100, 100))
    with open(filename) as f:
        y = 0
        for line in f.readlines():
            x = 0
            for c in line.strip():
                if c == '#':
                    grid[x][y] = 1
                x += 1
            y += 1
    return grid

def is_on(grid, x, y):
    w, h = grid.shape
    corners = ((0, 0), (0, h - 1), (w - 1, 0), (w - 1, h - 1))
    if (x, y) in corners:
        return 1
    return grid[x][y]

def neighbours(grid, x, y):
    w, h = grid.shape
    def valid(n):
        return n != (x, y) and n[0] < w and n[1] < h and n[0] >= 0 and n[1] >= 0
    neighbours = [n for n in it.product([x -1, x, x + 1], [y - 1, y, y + 1])
                  if valid(n)]
    return neighbours

def n_neighbours_on(grid, x, y):
    ns = neighbours(grid, x, y)
    stats = [is_on(grid, *n) for n in ns]
    return len([s for s in stats if s == 1])

def step(grid):
    w, h = grid.shape
    def transition(x, y):
        non = n_neighbours_on(grid, x, y)
        if non == 3:
            return True
        if non == 2 and grid[x][y] == 1:
            return True
        return False
    return np.array([[transition(x, y) for y in range(h)] for x in range(w)])

grid = parse_input(sys.argv[1])
for _ in range(100):
    grid = step(grid)
w, h = grid.shape
grid[0][0] = 1
grid[0][h - 1] = 1
grid[w - 1][0] = 1
grid[w - 1][h - 1] = 1
print(np.count_nonzero(grid))
