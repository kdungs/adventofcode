#!/usr/bin/env python3

with open("data/03.txt") as f:
    lines = f.readlines()

lines = [line.strip() for line in lines]

# Part 1
k = len(lines)
n = len(lines[0])
gamma = ""
epsilon = ""
for i in range(n):
    s = sum(int(line[i]) for line in lines)
    if s > k / 2:
        gamma += "1"
        epsilon += "0"
    else:
        gamma += "0"
        epsilon += "1"

gamma = int(gamma, 2)
epsilon = int(epsilon, 2)
print(gamma * epsilon)

# Part 2
oxy = lines
for i in range(n):
    k = len(oxy)
    if k == 1:
        break
    s = sum(int(o[i]) for o in oxy)
    most = "1"
    if s < k / 2:
        most = "0"
    oxy = [o for o in oxy if o[i] == most]

co2 = lines
for i in range(n):
    k = len(co2)
    if k == 1:
        break
    s = sum(int(c[i]) for c in co2)
    least = "0"
    if s < k / 2:
        least = "1"
    co2 = [c for c in co2 if c[i] == least]

oxy = int(oxy[0], 2)
co2 = int(co2[0], 2)
print(oxy * co2)
