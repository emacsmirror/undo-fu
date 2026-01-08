# SPDX-License-Identifier: GPL-2.0-or-later

import subprocess
import os
import sys


THIS_DIR = os.path.normpath(os.path.abspath(os.path.join(os.path.dirname(__file__))))
BASE_DIR = os.path.normpath(os.path.join(THIS_DIR, ".."))

EMACS_BIN = os.environ.get("EMACS_BIN") or "emacs"


def run_undo_fu_tests() -> int:
    cmd = [
        EMACS_BIN,
        "-batch",
        "-l", os.path.join(BASE_DIR, "undo-fu.el"),
        "-l", os.path.join(THIS_DIR, "undo-fu_tests.el"),
        "-f", "undo-fu_tests-run-all"
    ]

    process = subprocess.Popen(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    for line in process.stdout:
        sys.stdout.write(line)
        sys.stdout.flush()

    return process.wait()


def main() -> int:
    return run_undo_fu_tests()


if __name__ == "__main__":
    sys.exit(main())
