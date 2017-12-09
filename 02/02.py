#!/usr/bin/env python

import numpy as np
import itertools as it


def checksum(data):
  mins = data.min(1)
  maxs = data.max(1)
  diffs = maxs - mins
  return int(diffs.sum())


def evenlydivisiblesum(data):
  def perrow(row):
    pair = filter(lambda p: p[0] % p[1] == 0, it.permutations(row, 2))[0]
    return pair[0] / pair[1]

  return int(sum(map(perrow, data)))

if __name__ == '__main__':
  data = np.genfromtxt('input.txt')
  print(checksum(data))
  print(evenlydivisiblesum(data))
