#!/usr/bin/env python3

from typing import NamedTuple

import functools as ft
import itertools as it

import numpy as np
import scipy.spatial
import scipy.spatial.distance


# This took me way too long to derive myself.
# Note that the exercise text can be a bit confusing. At first I thought we
# would also have to take into account left-handed coordinate systems but that
# is not the case.
I = np.identity(3, dtype=int)
Rx = np.array([[1, 0, 0], [0, 0, -1], [0, 1, 0]])
Ry = np.array([[0, 0, 1], [0, 1, 0], [-1, 0, 0]])
Rz = np.array([[0, -1, 0], [1, 0, 0], [0, 0, 1]])
A = Rx @ Ry
A2 = A @ A
Rx2 = Rx @ Rx
Ry2 = Ry @ Ry
Rz2 = Rz @ Rz
B = Rz2 @ Ry
ORIENTATIONS = [
    L @ R
    for L, R in it.product(
        [I, A, A2],
        [I, Rx, Ry, Rz, Rx2, Ry2, Rz2, B],
    )
]

SIZE = 1000
CORNERS = np.array(list(it.product([-1, 1], repeat=3))) * SIZE


def parse(data):
    """Returns list of matrices where rows are x y z and cols are the beacons.
    One matrix corresponds to one scanner.
    """
    return [
        np.array([
            np.fromstring(p, sep=",", dtype=int)
            for p in s.split("\n")[1:]
        ])
        for s in (data + "\n").split("\n\n")[:-1]
    ]


def beacon_idx_closest_to_corners(beacons):
    kdtree = scipy.spatial.KDTree(beacons)
    return [
        kdtree.query(corner)[1]
        for corner in CORNERS
    ]


def count_overlapping_vectors(r1, r2):
    """Returns magnitude of set intersection of r1 and r2."""
    s1 = set(tuple(v) for v in r1)
    s2 = set(tuple(v) for v in r2)
    return len(s1 & s2)


class Overlap(NamedTuple):
    s1_idx: int
    s2_idx: int
    orient: np.ndarray
    b1: np.ndarray
    b2: np.ndarray


def find_overlap(s1, s2):
    """Returns orientation, and coordinates of overlapping beacons in their
    relative but oriented systems, if there's an overlap otherwise
    (None, None, None)."""
    s1_corners = beacon_idx_closest_to_corners(s1)
    for orient_idx, orient in enumerate(ORIENTATIONS):
        r2 = (orient @ s2.T).T
        for b1_idx in s1_corners:
            b1 = s1[b1_idx]
            for b2_idx, b2 in enumerate(r2):
                # Assume that r2 is oriented in the same way as s1 and that b1
                # and b2 refer to the same point in translated coordinate
                # systems.
                t2 = r2 - b2 + b1
                if count_overlapping_vectors(s1, t2) >= 12:
                    # We've found an overlap!
                    return (orient, b1, b2)
    return (None, None, None)


def find_overlaps(scanners):
    overlaps = []
    queue = [0]
    seen = set()
    while queue:
        s1_idx = queue.pop()
        s1 = scanners[s1_idx]
        seen.add(s1_idx)
        for s2_idx, s2 in enumerate(scanners):
            if s2_idx in seen:
                continue
            orient, b1, b2 = find_overlap(s1, s2)
            if orient is None:
                continue
            queue.append(s2_idx)
            overlaps.append(Overlap(
                s1_idx=s1_idx,
                s2_idx=s2_idx,
                orient=orient,
                b1=b1,
                b2=b2,
            ))
    return overlaps


def part1(overlaps):
    merged = {}
    for overlap in reversed(overlaps):
        s1 = merged.get(overlap.s1_idx, scanners[overlap.s1_idx])
        s2 = merged.get(overlap.s2_idx, scanners[overlap.s2_idx])
        r2 = (overlap.orient @ s2.T).T
        t2 = r2 - overlap.b2 + overlap.b1
        m = np.unique(np.append(s1, t2, axis=0), axis=0)
        merged[overlap.s1_idx] = m
    return len(merged[0])


def part2(overlaps):
    merged = {}
    for overlap in reversed(overlaps):
        s1 = merged.get(overlap.s1_idx, np.array([[0, 0, 0]]))
        s2 = merged.get(overlap.s2_idx, np.array([[0, 0, 0]]))
        r2 = (overlap.orient @ s2.T).T
        t2 = r2 - overlap.b2 + overlap.b1
        m = np.unique(np.append(s1, t2, axis=0), axis=0)
        merged[overlap.s1_idx] = m
    scanner = merged[0]
    dists = scipy.spatial.distance.cdist(scanner, scanner, metric="cityblock")
    return (int(np.max(dists)))


if __name__ == "__main__":
    with open("data/19.txt") as f:
        data = f.read()
    scanners = parse(data)
    overlaps = find_overlaps(scanners)
    print(part1(overlaps))
    print(part2(overlaps))
    # Could probably speed this up by first checking pairwise distancs before
    # checking all the orientations... ¯\_(ツ)_/¯
