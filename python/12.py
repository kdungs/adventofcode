import json
import sys

with open(sys.argv[1]) as f:
    data = json.load(f)


def sum_not_red(tree, total=0):
    vs = tree
    if type(tree) is dict:
        vs = tree.values()
        if any(v == 'red' for v in vs):
            return 0
    total += sum(v for v in vs if type(v) is int)
    total += sum(sum_not_red(v) for v in vs if type(v) in (list, dict))
    return total

print(sum_not_red(data))
