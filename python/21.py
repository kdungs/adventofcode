from collections import namedtuple
import itertools as it

Item = namedtuple('Item', ['name', 'cost', 'damage', 'armor'])
Char = namedtuple('Char', ['name', 'hp', 'damage', 'defense'])

weapons = [
    Item('Dagger',          8, 4, 0), 
    Item('Shortsword',     10, 5, 0), 
    Item('Warhammer',      25, 6, 0), 
    Item('Longsword',      40, 7, 0), 
    Item('Greataxe',       74, 8, 0)
]

armors = [
    Item('Naked',           0, 0, 0), 
    Item('Leather',        13, 0, 1), 
    Item('Chainmail',      31, 0, 2), 
    Item('Splintmail',     53, 0, 3), 
    Item('Bandedmail',     75, 0, 4), 
    Item('Platemail',     102, 0, 5)
]

rings = [
    Item('No Ring',         0, 0, 0), 
    Item('Defense +1',     20, 0, 1), 
    Item('Damage +1',      25, 1, 0), 
    Item('Defense +2',     40, 0, 2), 
    Item('Damage +2',      50, 2, 0), 
    Item('Defense +3',     80, 0, 3), 
    Item('Damage +3',     100, 3, 0)
]

ring_combinations = sorted(
    it.chain(
        [(rings[0], rings[0])],
        it.combinations(rings, 2)
    ),
    key=lambda rs: sum([r.cost for r in rs])
)

all_combinations = sorted(
    map(
        lambda cs: [cs[0], cs[1], cs[2][0], cs[2][1]],
        it.product(weapons, armors, ring_combinations),
    ),
    key=lambda cs: sum([c.cost for c in cs])
)

def attack(p1, p2):
    dmg = p1.damage - p2.defense
    if dmg < 1:
        dmg = 1
    return Char(p2.name, p2.hp - dmg, p2.damage, p2.defense), p1

def give_items(c, items):
    dmg = sum((i.damage for i in items))
    arm = sum((i.armor for i in items))
    return Char(c.name, c.hp, c.damage + dmg, c.defense + arm)

def fight_with_items(items):
    ps = (
        give_items(Char('player', 100, 0, 0), items),
        Char('boss', 104, 8, 1)
    )
    while ps[0].hp > 0:
        ps = attack(*ps)
    return ps[1].name == 'player'

def not_(f):
    def not_impl(*args, **kwargs):
        return not f(*args, **kwargs)
    return not_impl

ac = it.dropwhile(not_(fight_with_items), all_combinations)
print(sum([i.cost for i in next(ac)]))
