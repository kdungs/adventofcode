#!/usr/bin/env python3

from collections import Counter, defaultdict
from typing import NamedTuple
import itertools as it

with open("data/21.txt") as f:
    lines = f.readlines()

positions = [
    int(line.split(":")[-1]) - 1
    for line in lines
]


class GameState(NamedTuple):
    pos1: int
    scr1: int
    pos2: int
    scr2: int

    @classmethod
    def create(cls, pos1, pos2):
        return cls(pos1, 0, pos2, 0)

    def move(self, p1, amount):
        if p1:
            pos = (self.pos1 + amount) % 10
            scr = self.scr1 + pos + 1
            return GameState(pos, scr, self.pos2, self.scr2)
        pos = (self.pos2 + amount) % 10
        scr = self.scr2 + pos + 1
        return GameState(self.pos1, self.scr1, pos, scr)

    def score(self, p1):
        if p1:
            return self.scr1
        return self.scr2

# Part 1
state = GameState.create(*positions)
p1 = True
die = it.cycle(range(1, 101))
nrolls = 0
while state.scr1 < 1000 and state.scr2 < 1000:
    state = state.move(p1, next(die) + next(die) + next(die))
    nrolls += 3
    p1 = not p1
print(nrolls * min(state.scr1, state.scr2))

# Part 2
victories = [0, 0]
states = {GameState.create(*positions): 1}
p1 = True
all_rolls = Counter(sum(dice) for dice in it.product([1,2,3], repeat=3))
while len(states) > 0:
    next_states = defaultdict(int)
    for state, cnt in states.items():
        for amount, num in all_rolls.items():
            nstate = state.move(p1, amount)
            count = cnt * num
            if nstate.score(p1) >= 21:
                victories[int(p1)] += count
            else:
                next_states[nstate] += count
    p1 = not p1
    states = next_states
print(max(victories))
