import itertools as it 
import re
import sys

def parse_file(filename):
    reg = re.compile(r'^(\w+) would (lose|gain) (\d+) happiness units by sitting next to (\w+)\.$')
    with open(filename) as f:
        mats = (reg.match(line) for line in f.readlines())
    mapping = {}
    for mat in filter(None, mats):
        a, lg, h, b = mat.groups()
        if not mapping.has_key(a):
            mapping[a] = {}
        mapping[a][b] = (-1 if lg == 'lose' else 1) * int(h)
    return mapping

def iterate_triplets(iterable):
    a, b, c = it.tee(iterable, 3)
    b = it.cycle(b)
    c = it.cycle(c)
    next(c)
    next(c)
    next(b)
    return it.izip(a, b, c)

def find_ideal_seating(mapping):
    people = mapping.keys()
    def happiness(seating):
        def happiness_per_triplet(triplet):
            left, middle, right = triplet
            return mapping[middle][left] + mapping[middle][right]
        groups = iterate_triplets(seating)
        return sum(happiness_per_triplet(g) for g in groups)

    return max(happiness(seating) for seating in it.permutations(people))

def add_yourself(people):
    names = people.keys()
    for p in people.values():
        p['yourself'] = 0
    people['yourself'] = {p: 0 for p in names}
    return people

data = parse_file(sys.argv[1])
print(find_ideal_seating(data))
print(find_ideal_seating(add_yourself(data)))
