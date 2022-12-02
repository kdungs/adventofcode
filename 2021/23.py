#!/usr/bin/env python3

# Part 1: manual

steps = [
    "Bx2",
    "Ax5",
    "Bx3",
    "Cx2",
    "Bx5",
    "Cx3",
    "Cx4",
    "Ax3",
    "Dx9",
    "Dx9",
    "Ax3",
    "Ax8",
]
costs = {
    "A": 1,
    "B": 10,
    "C": 100,
    "D": 1000,
}
total = 0
for step in steps:
    amph, num = step.split("x")
    total += costs[amph] * int(num)
print(total)
