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

def make_arrays_w_calories(data):
    return [np.array(d) for d in data]

def all_combinations(m, n):
    if m == 0:
        yield [0] * n
    elif n == 1:
        yield [m]
    else:
        for x in range(0, m + 1):
            for c in all_combinations(m - x, n - 1):
                yield [x] + c

def cookie_rater_wo_calories(data):
    ingredients = make_arrays_wo_calories(data)
    def rate_impl(counts):
        xs = sum([c * i for c, i in zip(counts, ingredients)])
        return reduce(op.mul, [x if x > 0 else 0 for x in xs], 1)
    return rate_impl

def cookie_rater_w_calories(data):
    ingredients = make_arrays_w_calories(data)
    def rate_impl(counts):
        xs = sum([c * i for c, i in zip(counts, ingredients)])
        if xs[-1] != 500:
            return 0
        return reduce(op.mul, [x if x > 0 else 0 for x in xs[:-1]], 1)
    return rate_impl

def find_best_cookie(rater, m=100, n=4):
    return max((rater(np.array(counts)) for counts in all_combinations(m, n)))


data = load_data(sys.argv[1])
print(find_best_cookie(cookie_rater_wo_calories(data)))
print(find_best_cookie(cookie_rater_w_calories(data)))
