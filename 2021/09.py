#!/usr/bin/env python3

import heapq

# Deliberately not using numpy because dependencies...

with open("data/09.txt") as f:
    lines = f.readlines()

matrix = [[int(x) for x in line.strip()] for line in lines]
rows = len(matrix)
cols = len(matrix[0])


def outside(x, y):
    return (y < 0
         or y >= rows
         or x < 0
         or x >= cols)


# Part 1
def get(x, y):
    if outside(x, y):
        return 10  # Always larger than single digit
    return matrix[y][x]


risklevel = 0
for y in range(rows):
    for x in range(cols):
        neighbours = [
            get(x, y - 1),
            get(x, y + 1),
            get(x - 1, y),
            get(x + 1, y),
        ]
        h = get(x, y)
        if all(n > h for n in neighbours):
            risklevel += h + 1
print(risklevel)


# Part 2
basins = [[x != 9 for x in row] for row in matrix]

# Worst floodfill you've ever seen. YOLO
def floodfill(x, y):
    if outside(x, y):
        return 0
    if not basins[y][x]:
        return 0
    basins[y][x] = False
    return (1
          + floodfill(x + 1, y)
          + floodfill(x, y + 1)
          + floodfill(x - 1, y)
          + floodfill(x, y - 1))

sizes = []
for y in range(rows):
    for x in range(cols):
        size = floodfill(x, y)
        if size > 0:
            sizes.append(size)

l3 = heapq.nlargest(3, sizes)
print(l3[0] * l3[1] * l3[2])
