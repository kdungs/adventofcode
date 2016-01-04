import numpy as np
import operator as op
import re
import sys

def load_data(filename):
    with open(filename) as f:
        return [[int(x) for x in re.findall(r'(-?\d+)', line)]
              for line in f.readlines()]

def make_arrays_wo_calories(data):
    return [np.array(d[:-1]) for d in data]

def all_combinations(m, n):
    if m == 0:
        yield [0] * n
    elif n == 1:
        yield [m]
    else:
        for x in range(0, m + 1):
            for c in all_combinations(m - x, n - 1):
                yield [x] + c

def cookie_rater(ingredients):
    def rate_impl(counts):
        xs = sum([c * i for c, i in zip(counts, ingredients)])
        return reduce(op.mul, [x if x > 0 else 0 for x in xs], 1)
    return rate_impl


data = load_data(sys.argv[1])

rate = cookie_rater(make_arrays_wo_calories(data))
print(max((rate(np.array(counts)) for counts in all_combinations(100, 4))))
