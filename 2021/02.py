#!/usr/bin/env python3

with open("data/02.txt") as f:
    lines = f.readlines()

# Part 1
dirs = {
    "forward": (1, 0),
    "down": (0, 1),
    "up": (0, -1),
}

p, d = 0, 0
for line in lines:
    c, n = line.split(" ")
    n = int(n)
    dp, dd = dirs[c]
    p += n * dp
    d += n * dd
print(p * d)

# Part 2
class Ship:
    def __init__(self, aim, pos, depth):
        self.aim = aim
        self.pos = pos
        self.depth = depth

    def forward(self, n):
        self.pos += n
        self.depth += self.aim * n

    def down(self, n):
        self.aim += n

    def up(self, n):
        self.aim -= n


s = Ship(0, 0, 0)
for line in lines:
    c, n = line.split(" ")
    n = int(n)
    f = getattr(s, c)
    f(n)

print(s.pos * s.depth)
