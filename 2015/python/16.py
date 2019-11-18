import re
import sys

def parse_line(line):
    reg = re.compile(r'(\w+): (\d+)')
    sue = dict(reg.findall(line))
    sue = {k: int(v) for k, v in sue.items()}
    sue['name'] = line.partition(':')[0]
    return sue 

def parse_input(filename):
    with open(filename) as f:
        return [parse_line(l) for l in f.readlines()]

def rule_out(sues, prop, value):
    return filter(lambda sue: sue.get(prop, value) == value, sues)

def use_knowledge(sues, knowledge):
    for p, v in knowledge.items():
        sues = rule_out(sues, p, v)
    return sues

def use_approximate_knowledge(sues, knowledge):
    for p, v in knowledge.items():
        if p in ('cats', 'trees'):
            sues = filter(lambda sue: sue.get(p, v + 1) > v, sues)
        elif p in ('pomeranians', 'goldfish'):
            sues = filter(lambda sue: sue.get(p, v - 1) < v, sues)
        else:
            sues = rule_out(sues, p, v)
    return sues

knowledge = {
    'akitas': 0,
    'cars': 2,
    'cats': 7,
    'children': 3,
    'goldfish': 5,
    'perfumes': 1,
    'pomeranians': 3,
    'samoyeds': 2,
    'trees': 3,
    'vizslas': 0
}

sues = parse_input(sys.argv[1])
print(use_knowledge(sues, knowledge))
print(use_approximate_knowledge(sues, knowledge))
