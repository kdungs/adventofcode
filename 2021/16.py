#!/usr/bin/env python3

# This is a really, really nice binary encoding exercise that would be a lot of
# fun in C or a similar language.
# But this is Python, so let's format some strings LOL

from __future__ import annotations
from dataclasses import dataclass
from typing import Iterable, List, Optional
import itertools as it
import functools as ft
import operator as op


def stream(hexdata: str) -> Iterable[str]:
    for hexchar in hexdata:
        binary = f"{int(hexchar, 16):04b}"
        yield from binary


def read(n: int, binstream: Iterable[str]) -> int:
    return int("".join(it.islice(binstream, n)), 2)


@dataclass
class Packet:
    version: int
    type_id: int
    value: Optional[int]
    sub: List[Packet]


def read_packet(binstream: Iterable[str]) -> Packet:
    version = read(3, binstream)
    type_id = read(3, binstream)
    if type_id == 4:
        num = 0
        more = True
        while more:
            more = bool(read(1, binstream))
            num = (num * 16) + read(4, binstream)
        return Packet(version, type_id, num, [])
    # Operator packet
    length_type_id = read(1, binstream)
    subs = []
    if length_type_id == 0:
        len_sub = read(15, binstream)
        substr = it.islice(binstream, len_sub)
        more = True
        while more:
            try:
                subs.append(read_packet(substr))
            except:
                more = False
    else:
        num_subs = read(11, binstream)
        for _ in range(num_subs):
            subs.append(read_packet(binstream))
    return Packet(version, type_id, 0, subs)


def read_packets(binstream: Iterable[str]) -> Iterable[Packet]:
    while True:
        try:
            yield read_packet(binstream, padded=True)
        except Exception as e:
            break



def test():
    cases = [
        {
            "input": "110100101111111000101000",
            "expected": Packet(6, 4, 2021, []),
        },
        {
            "input": "00111000000000000110111101000101001010010001001000000000",
            "expected": Packet(1, 6, 0, [
                Packet(6, 4, 10, []),
                Packet(2, 4, 20, [] ),
            ]),
        },
        {
            "input": "11101110000000001101010000001100100000100011000001100000",
            "expected": Packet(7, 3, 0, [
                Packet(2, 4, 1, []),
                Packet(4, 4, 2, []),
                Packet(1, 4, 3, []),
            ]),
        },
    ]
    for case in cases:
        i = case["input"]
        r = read_packet(iter(i))
        e = case["expected"]
        if r != e:
            print(f"FAILED for input '{i}': {r} != {e}")



def version_sum(packet: Packet) -> int:
    return packet.version + sum(version_sum(p) for p in packet.sub)


def version_sums(ps: Iterable[Packet]) -> int:
    return sum(version_sum(p) for p in ps)


def evaluate(p: Packet) -> int:
    if p.type_id == 4:
        return p.value
    return {
        0: sum,
        1: lambda vs: ft.reduce(op.mul, vs, 1),
        2: min,
        3: max,
        5: lambda vs: int(next(vs) > next(vs)),
        6: lambda vs: int(next(vs) < next(vs)),
        7: lambda vs: int(next(vs) == next(vs)),
    }[p.type_id](evaluate(s) for s in p.sub)


if __name__ == "__main__":
    test()

    with open("data/16.txt") as f:
        hexdata = f.read().strip()

    p = read_packet(stream(hexdata))
    # Part 1
    print(version_sum(p))
    # Part 2
    print(evaluate(p))

