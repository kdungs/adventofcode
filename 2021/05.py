#!/usr/bin/env python3

from collections import defaultdict

with open("data/05.txt") as f:
    lines = f.readlines()


points = defaultdict(int)
for line in lines:
    left, right = line.split(" -> ")
    x1, y1 = map(int, left.split(","))
    x2, y2 = map(int, right.split(","))
    if x1 != x2 and y1 != y2:
        # not a horizontal or vertical line
        continue
    for x in range(min(x1, x2), max(x1, x2) + 1):
        for y in range(min(y1, y2), max(y1, y2) + 1):
            points[(x, y)] += 1

intersections = sum(v > 1 for v in points.values())
print(intersections)


# Part 2
points = defaultdict(int)
for line in lines:
    left, right = line.split(" -> ")
    x1, y1 = map(int, left.split(","))
    x2, y2 = map(int, right.split(","))
    if x1 == x2:
        # Vertical
        x = x1
        for y in range(min(y1, y2), max(y1, y2) + 1):
            points[(x, y)] += 1
    elif y1 == y2:
        # Horizontal
        y = y1
        for x in range(min(x1, x2), max(x1, x2) + 1):
            points[(x, y)] += 1
    else:
        # Diagonal (by definition)
        # Verify!
        dx = x2 - x1
        dy = y2 - y1
        assert(abs(dy / dx) == 1)
        sx = dx / abs(dx)
        sy = dy / abs(dy)
        for i in range(abs(dx) + 1):
                x = x1 + sx * i
                y = y1 + sy * i
                points[(x, y)] += 1

intersections = sum(v > 1 for v in points.values())
print(intersections)
