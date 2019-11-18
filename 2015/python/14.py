from collections import (namedtuple, defaultdict)
import re
import sys

Reindeer = namedtuple('Reindeer', ['name', 'velocity', 'stamina', 'rest'])

def parse_input(filename):
    reg = re.compile(r'^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.$')
    with open(filename) as f:
        mats = (reg.match(line) for line in f.readlines())
    return [Reindeer(m.group(1), int(m.group(2)), int(m.group(3)), int(m.group(4))) for m in mats]


def distance_after_time(reindeer, time):
    dist_per_cycle = reindeer.velocity * reindeer.stamina
    time_per_cycle = reindeer.stamina + reindeer.rest
    
    full_cycles = time // time_per_cycle
    rest_time = time % time_per_cycle
    if rest_time > reindeer.stamina:
        rest_time = reindeer.stamina

    return full_cycles * dist_per_cycle + rest_time * reindeer.velocity

def reindeers_in_lead(reindeers, time):
    dists = [(distance_after_time(r, time), r) for r in reindeers]
    max_dist = max(d[0] for d in dists)
    return filter(lambda d: d[0] == max_dist, dists)

def scores(reindeers, time_limit):
    scores = defaultdict(int)
    for time in range(1, time_limit + 1):
        leaders = reindeers_in_lead(reindeers, time)
        for leader in leaders:
            scores[leader[1]] += 1
    return scores

TIME = 2503

reindeers = parse_input(sys.argv[1])
print(reindeers_in_lead(reindeers, TIME))
print(max(scores(reindeers, TIME).values()))
