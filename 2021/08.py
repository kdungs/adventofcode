#!/usr/bin/env python3

def parse(line):
    ps, vs = line.split(" | ")
    # Order of letters does not matter; can use sets for easier intersections.
    return (
        [set(p) for p in ps.strip().split(" ")],
        [set(v) for v in vs.strip().split(" ")]
    )


def of(xs, size=None, sub=None, nsub=None, neqs=None):
    if size is not None:
        xs = [x for x in xs if len(x) == size]
    if sub is not None:
        xs = [x for x in xs if sub.issubset(x)]
    if nsub is not None:
        xs = [x for x in xs if not nsub.issubset(x)]
    if neqs is not None:
        xs = [x for x in xs if not x in neqs]
    return xs[0]


def infer(ps, vs):
    # Successively build lookup map by inferring information.
    # We always just need to find a way to discern the digit we are looking for
    # from the others of the same length.
    """
    abcdefg : 8

    abc efg : 0
    ab defg : 6
    abcd fg : 9

    a cde g : 2
    a cd fg : 3
    ab d fg : 5

     bcd f  : 4
    a c  f  : 7
      c  f  : 1
    """
    one = of(ps, size=2)
    four = of(ps, size=4)
    seven = of(ps, size=3)
    eight = of(ps, size=7)
    three = of(ps, size=5, sub=one)
    five = of(ps, size=5, sub=(four - one))
    two = of(ps, size=5, neqs=[three, five])
    nine = of(ps, size=6, sub=four)
    six = of(ps, size=6, nsub=seven)
    zero = of(ps, size=6, neqs=[nine, six])
    # Build a lookup table; have to use strings as keys since sets are not
    # hashable...
    def k(xs):
        return "".join(sorted(list(xs)))

    digits = {
        k(zero): 0,
        k(one): 1,
        k(two): 2,
        k(three): 3,
        k(four): 4,
        k(five): 5,
        k(six): 6,
        k(seven): 7,
        k(eight): 8,
        k(nine): 9,
    }
    return [digits[k(v)] for v in vs]


with open("data/08.txt") as f:
    lines = f.readlines()

# Part 1
cnt = 0
for line in lines:
    _, vs = parse(line)
    for v in vs:
        if len(v) in (2, 3, 4, 7):
            cnt += 1
print(cnt)

# Part 2
total = 0
for line in lines:
    ps, vs = parse(line)
    digits = infer(ps, vs)
    total += int("".join(str(d) for d in digits))
print(total)
