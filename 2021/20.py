#!/usr/bin/env python3

# Hope you didn't expect me to write nice code for this one.

PADDING = 4
OFFSETS = [
    (-1, -1),
    (0, -1),
    (1, -1),
    (-1, 0),
    (0, 0),
    (1, 0),
    (-1, 1),
    (0, 1),
    (1, 1),
]


with open("data/20.txt") as f:
    lines = [line.strip() for line in f.readlines()]

enhance = lines[0] # First element is '#', thanks!
image = lines[2:]

# Part 1
# Step 1: Need to calculate the pixels that stay off in this generation.
def cycle(image):
    ones = {
        (x, y)
        for y, row in enumerate(image)
        for x, c in enumerate(row)
        if c == "#"
    }
    off = set()
    for y in range(-PADDING, len(image) + PADDING):
        for x in range(-PADDING, len(image[0]) + PADDING):
            bits = [(x + dx, y + dy) in ones for (dx, dy) in OFFSETS]
            addr = int("".join(str(int(b)) for b in bits), 2)
            if enhance[addr] != "#":
                off.add((x, y))
    # Step 2: Now it's the other way around.
    minx = min(x for (x, _) in off)
    maxx = max(x for (x, _) in off)
    miny = min(y for (_, y) in off)
    maxy = max(y for (_, y) in off)
    result = []
    for y in range(miny - PADDING, maxy + PADDING):
        row = []
        for x in range(minx - PADDING, maxx + PADDING):
            bits = [(x + dx, y + dy) not in off for (dx, dy) in OFFSETS]
            addr = int("".join(str(int(b)) for b in bits), 2)
            row.append(enhance[addr])
        result.append("".join(row))
    return result


def cnt(image):
    return sum(sum(int(c == "#") for c in row) for row in image)

# Part 1
print(cnt(cycle(image)))

# Part 2
for _ in range(25):
    image = cycle(image)
print(cnt(image))
