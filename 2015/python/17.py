import sys
import itertools as it

def parse_input(filename):
    with open(filename) as f:
        return [int(l) for l in f.readlines()]

def n_combs(containers, content):
    if content == 0:
        return 1
    if content < 0 or len(containers) == 0:
        return 0
    possible = list(filter(lambda c: c <= content, containers))
    s = 0
    while possible:
        p = possible[0]
        possible = possible[1:]
        s += n_combs(possible, content - p)
    return s

def num_min_containers(containers, content):
    if content == 0:
        return (0, 1)
    if content < 0 or len(containers) == 0:
        return None
    ps = [c for c in containers if c <= content]
    subs = []
    while ps:
        p = ps[0]
        ps = ps[1:]
        res = num_min_containers(ps, content - p)
        if res is not None:
            subs.append(res)
    if len(subs) == 0:
        return None
    minlen = min(subs, key=lambda s: s[0])[0]
    ways = sum(s[1] for s in subs if s[0] == minlen)
    return (minlen + 1, ways)

content = 150
data = list(reversed(sorted(parse_input(sys.argv[1]))))
print(n_combs(data, content))
print(num_min_containers(data, content))

