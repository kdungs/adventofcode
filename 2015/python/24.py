import itertools as it
import sys

def load_data(filename):
    with open(filename) as f:
        return {int(line) for line in f.readlines()}

def qe(xs):
    return reduce(lambda acc, x: acc * x, xs, 1)

def find_smalles_qe(xs, compartments):
    a = sum(xs) / compartments
    for i in range(1, len(xs)):
        cs = [c for c in it.combinations(xs, i) if sum(c) == a]
        if cs:
            return min([qe(c) for c in cs])


data = load_data(sys.argv[1])
print(find_smalles_qe(data, 3))
print(find_smalles_qe(data, 4))
