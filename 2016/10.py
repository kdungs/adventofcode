import sys


class Problem:
    def __init__(self):
        self._map = {}

    def _add_out_node_if_needed(self, x):
        if x.startswith("output"):
            self._map[x] = {"ins": []}

    def add_line(self, line: str):
        if line.startswith("value"):
            v, target = line.split(" goes to ")
            self._map[v] = {"in": int(v.replace("value ", "")),
                            "out": target}
            self._add_out_node_if_needed(target)
        if line.startswith("bot"):
            b, targets = line.split(" gives low to ")
            lo, hi = targets.split(" and high to ")
            self._map[b] = {"out": {"lo": lo, "hi": hi},
                            "ins": []}
            self._add_out_node_if_needed(lo)
            self._add_out_node_if_needed(hi)

    def _needs_processing(self, x):
        if not x.startswith("bot"):
            return False
        return len(self._map[x]["ins"]) == 2

    def solve(self):
        queue = [k for k in self._map.keys() if k.startswith("value")]
        while queue:
            elem, queue = queue[0], queue[1:]
            info = self._map[elem]
            if elem.startswith("value"):
                target = info["out"]
                self._map[target]["ins"].append(info["in"])
                if self._needs_processing(target):
                    queue.append(target)
            if elem.startswith("bot"):
                vlo, vhi = sorted(info["ins"])
                lo = info["out"]["lo"]
                hi = info["out"]["hi"]
                self._map[lo]["ins"].append(vlo)
                self._map[hi]["ins"].append(vhi)
                if self._needs_processing(lo):
                    queue.append(lo)
                if self._needs_processing(hi):
                    queue.append(hi)

    def find_bot_with(self, ins) -> str:
        for name, info in self._map.items():
            if not name.startswith("bot"):
                continue
            if ins == sorted(info["ins"]):
                return name
        return "NONE"

    def get_output(self, x) -> int:
        return self._map[f"output {x}"]["ins"][0]


if __name__ == "__main__":
    p = Problem()
    for line in sys.stdin.readlines():
        p.add_line(line.strip())
    p.solve()
    print(p._map)
    print(p.find_bot_with([17, 61]))
    print(p.get_output(0) * p.get_output(1) * p.get_output(2))
