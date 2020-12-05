# This is the implementation of a Bazel extra_action which genenerates
# _compile_command files for generate_compile_commands.py to consume.

from __future__ import annotations
import json
import logging
import sys
from typing import NamedTuple

import third_party.bazel.protos.extra_actions_base_pb2 as pb


LOG = logging.getLogger("CompileCommand")


class CompileCommand(NamedTuple):
    directory: str
    command: str
    file: str

    def serialize_to_file(self, fname: str):
        with open(fname, "w") as f:
            json.dump(self._asdict(), f)

    @classmethod
    def from_action(cls, action: pb.ExtraActionInfo) -> CompileCommand:
        cinfo = action.Extensions[pb.CppCompileInfo.cpp_compile_info]
        compiler = cinfo.tool
        options = " ".join(cinfo.compiler_option)
        return cls(
            directory="",
            command=f"{compiler} {options}",
            file=f"{cinfo.source_file}",
        )


def action_from_file(fname: str) -> pb.ExtraActionInfo:
    action = pb.ExtraActionInfo()
    with open(fname, "rb") as f:
        action.MergeFromString(f.read())
    return action


def main(args) -> int:
    src = args[1]
    dst = args[2]
    LOG.debug(f"FROM: {src}")
    LOG.debug(f"  TO: {dst}")
    action = action_from_file(src)
    LOG.debug(f" ACT: {action}")
    command = CompileCommand.from_action(action)
    LOG.debug(f" CMD: {command}")
    command.serialize_to_file(dst)
    return 0


if __name__ == "__main__":
    # logging.basicConfig(level=logging.DEBUG)
    sys.exit(main(sys.argv))
