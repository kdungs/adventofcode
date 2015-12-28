from collections import defaultdict
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


mappings, instr = parse_file(sys.argv[1])

print(len(individual_new_strings(forward_mapping(mappings), instr)))
