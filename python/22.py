from collections import namedtuple
from copy import deepcopy
import functools as ft
import itertools as it

Player = namedtuple('Player', ['hp', 'mana', 'armor'])
Boss = namedtuple('Boss', ['hp', 'dmg']) 

def compose(f, g):
    def compose_impl(*args, **kwargs):
        return f(*g(*args, **kwargs))

def damage_spell(damage):
    def damage_spell_impl(player, boss):
        return (player, Boss(boss.hp - damage, boss.dmg))
    return damage_spell_impl

def heal_spell(healing):
    def heal_spell_impl(player, boss):
        return (Player(player.hp + healing, player.mana, player.armor), boss)
    return heal_spell_impl

def mana_spell(mana):
    def mana_spell_impl(player, boss):
        return (Player(player.hp, player.mana + mana, player.armor), boss)
    return mana_spell_impl

def armor_spell(armor):
    def armor_spell_impl(player, boss):
        return (Player(player.hp, player.mana, player.armor + armor), boss)
    return armor_spell_impl

def boss_damage(player, boss):
    damage = boss.dmg - player.armor
    if damage < 1:
        damage = 1
    return (Player(player.hp - damage, player.mana, player.armor), boss)

spells = {
    'Magic Missile': {
        'cost': 53,
        'instant': damage_spell(4)
    },
    'Drain': {
        'cost': 73,
        'instant': compose(damage_spell(2), heal_spell(2))
    },
    'Shield': {
        'cost': 113,
        'turns': 6,
        'instant': armor_spell(7),
        'cleanup': armor_spell(-7)
    },
    'Poison': {
        'cost': 173,
        'turns': 6,
        'effect': damage_spell(3)
    },
    'Recharge': {
        'cost': 229,
        'turns': 5,
        'effect': mana_spell(101)
    }
}

class FightState(object):
    FIGHTING = 0
    PLAYER_WON = 1
    PLAYER_LOST = 2

class Fight(object):
    def __init__(self, difficulty, boss_hp=55, boss_dmg=8):
        self.difficulty = difficulty
        self.state = FightState.FIGHTING
        self.totalManaSpent = 0
        self.player = Player(50, 500, 0)
        self.boss = Boss(boss_hp, boss_dmg)
        self.effects = {}
        self.cleanups = {}

    def set_state(self, state):
        """ Make sure whatever state (≠ FIGHTING) is set first is kept. """
        if self.state != FightState.FIGHTING:
            return
        self.state = state

    def apply_(self, f):
        self.player, self.boss = f(self.player, self.boss)
        if self.player.hp <= 0:
            self.set_state(FightState.PLAYER_LOST)
        if self.boss.hp <= 0:
            self.set_state(FightState.PLAYER_WON)

    def apply_effects(self):
        for effect in self.effects:
            self.apply_(effect)
        self.effects = {effect: timer - 1
                        for effect, timer in self.effects.items()
                        if timer - 1 > 0}

    def apply_cleanups(self):
        cleanupNow = [cleanup for cleanup, timer in self.cleanups.items()
                      if timer == 0]
        self.cleanups = {cleanup: timer - 1
                         for cleanup, timer in self.cleanups.items()
                         if timer > 0}
        for cleanup in cleanupNow:
            self.apply_(cleanup)

    def cast_spell(self, spell):
        self.totalManaSpent += spell['cost']
        self.apply_(mana_spell(-spell['cost']))
        if self.player.mana < 0:
            self.set_state(FightState.PLAYER_LOST)
            return
        instant = spell.get('instant')
        if instant:
            self.apply_(instant)
        turns = spell.get('turns')
        if turns:
            effect = spell.get('effect')
            if effect:
                self.effects[effect] = turns
            cleanup = spell.get('cleanup')
            if cleanup:
                self.cleanups[cleanup] = turns

    def spell_in_use(self, spell):
        if not spell.has_key('turns'):
            return False
        if spell.has_key('effect') and self.effects.has_key(spell['effect']):
            return True
        if spell.has_key('cleanup') and self.cleanups.has_key(spell['cleanup']):
            return True
        return False

    def allowed_spells(self):
        return [s for s in spells.values()
                if not self.spell_in_use(s) and self.player.mana >= s['cost']]

    def start_of_turn(self): 
        self.apply_effects()
        self.apply_cleanups()

    def hard_mode(self):
        if self.difficulty == 'hard':
            self.apply_(heal_spell(-1))

    def fight_round(self):
        self.hard_mode()
        self.start_of_turn()  # Player's turn 
        if self.state != FightState.FIGHTING:
            return [self]
        copies = [(spell, deepcopy(self)) for spell in self.allowed_spells()]
        for spell, copy in copies:
            copy.cast_spell(spell)
            copy.start_of_turn()  # Boss' turn
            copy.apply_(boss_damage)
        return [copy for _, copy in copies]


def flatten(listOfLists):
    "Flatten one level of nesting"
    return it.chain.from_iterable(listOfLists)

def find_min_mana(difficulty):
    fights = [Fight(difficulty)]
    minMana = 999999
    while fights:
        fs = list(flatten([f.fight_round() for f in fights]))
        won = [f for f in fs if f.state == FightState.PLAYER_WON]
        if won:
            minMana = min(minMana, min([f.totalManaSpent for f in won]))
        fights = [f for f in fs
                  if f.state == FightState.FIGHTING
                     and f.totalManaSpent < minMana]
    return minMana

print(find_min_mana('easy'))
print(find_min_mana('hard'))
