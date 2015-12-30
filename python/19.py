from collections import defaultdict, OrderedDict
import itertools as it
import re
import sys

def mapping_from_line(line):
    f, t = line.strip().split(' => ')
    return f, t

def mapping_from_lines(lines):
    return [mapping_from_line(line) for line in lines]

def parse_file(filename):
    with open(filename) as f:
        lines = f.readlines()
        return mapping_from_lines(lines[:-2]), lines[-1].strip()

def forward_mapping(mappings):
    mps = defaultdict(list)
    for k, v in mappings:
        mps[k].append(v)
    return mps

def backward_mapping(mappings):
    return {v: k for k, v in mappings}

def interspersed_(ss, k, v):
    return [k.join(ss[:i]) + v + k.join(ss[i:]) for i in range(1, len(ss))]

def individual_new_strings(fwdmap, s):
    strs = set()
    for k, vs in fwdmap.items():
        ss = s.split(k)
        strs.update(it.chain(*[interspersed_(ss, k, v) for v in vs]))
    return strs
    
def individual_atoms(s):
    return set(re.findall(r'[A-Z][a-z]?', s))

def find_bad_atoms(bwdmap):
    bad = set()
    for k in bwdmap.keys():
        bad_atoms = set(filter(lambda a: a not in bwdmap.values(),
                               individual_atoms(k)))
        bad.update(bad_atoms)
    return bad

def split_on_bad_atoms(bwdmap, bad_atoms=None):
    if bad_atoms is None:
        bad_atoms = find_bad_atoms(bwdmap)
    def has_bad_atoms(s):
        return any(bad_guy in individual_atoms(s) for bad_guy in bad_atoms)
    god = {k: v for k, v in bwdmap.items() if not has_bad_atoms(k)}
    bad = {k: v for k, v in bwdmap.items() if has_bad_atoms(k)}
    return god, bad

def reduce_bruteforce(bwdmap, s, i=0):
    count = 0
    while any(k in s for k in bwdmap.keys()):
        for k, v in bwdmap.items():
            s, c = re.subn(k, v, s)
            count += c
    return s, count + i

def reduce_molecule(bwdmap, s, i=0):
    bad_atoms = find_bad_atoms(bwdmap)
    god, bad = split_on_bad_atoms(bwdmap, bad_atoms)
    for k, v in bad.items():
        if k in s:
            break
    splitter = re.compile(k)
    splits = splitter.split(s)
    rsplits = [reduce_bruteforce(bwdmap, s) for s in splits]
    rcount = sum([x[1] for x in rsplits])
    rstr = v.join([x[0] for x in rsplits])
    return rstr, (i + rcount + len(splits) - 1)


if __name__ == '__main__':
    mappings, instr = parse_file(sys.argv[1])
    print(len(individual_new_strings(forward_mapping(mappings), instr)))
    ## 
    bwdmap = backward_mapping(mappings)
    x, i = reduce_molecule(bwdmap, instr)
    print(x, i)
    #x, i = reduce_molecule(bwdmap, x, i)
    #print(x, i)
    x, i = reduce_bruteforce(bwdmap, x, i)
    print(x, i)
