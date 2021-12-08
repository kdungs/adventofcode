#!/usr/bin/env python3

with open('data/01.txt') as f:
    lines = f.readlines()

nums = [int(line) for line in lines]

# Part 1
result = sum(1 if a < b else 0 for a, b in zip(nums, nums[1:]))
print(result)

# Part 2
windows = [a + b + c for a, b, c in zip(nums, nums[1:], nums[2:])]
result = sum(1 if a < b else 0 for a, b in zip(windows, windows[1:]))
print(result)
