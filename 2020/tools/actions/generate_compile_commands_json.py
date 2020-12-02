#!/usr/bin/env python3

import json
import pathlib
import subprocess
import sys
from typing import Dict, Iterable


HERE = pathlib.Path(__file__).parent.absolute()
ROOT = HERE / "../../"


def get_bazel_execroot() -> pathlib.Path:
    execroot = subprocess.check_output(
        ["bazel", "info", "execution_root"],
        cwd=ROOT,
    )
    return pathlib.Path(execroot.decode("utf-8").rstrip())


def get_action_path() -> pathlib.Path:
    # need to do some magic because bazel-bin is a symlink
    return (
        ROOT /
        "bazel-bin/../extra_actions/tools/actions/generate_compile_commands_action"
    ).resolve()


def read_compile_command(fname: str) -> Dict[str, str]:
    with open(fname, "r") as f:
        return json.load(f)


def read_compile_commands(path: pathlib.Path) -> Iterable[Dict[str, str]]:
    for f in path.rglob("*compile_command.json"):
        yield read_compile_command(f)


def with_execroot(cmds: Iterable[Dict[str, str]]) -> Iterable[Dict[str, str]]:
    execroot = get_bazel_execroot()
    for cmd in cmds:
        cmd["directory"] = f"{execroot}"
        yield cmd


def write_compile_commands_json(
    dst: pathlib.Path,
    commands: Iterable[Dict[str, str]],
):
    cmds = list(commands)
    with open(dst / "compile_commands.json", "w") as f:
        json.dump(cmds, f)


def main(args) -> int:
    path = get_action_path()
    cmds = read_compile_commands(path)
    cmds = with_execroot(cmds)
    write_compile_commands_json(ROOT, cmds)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
