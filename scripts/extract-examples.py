#!/bin/sh
#
# Extract example programs.
#
# Requirements:
#   python >= 3.6.
#
""":" .

exec python3 "$0" "$@"
"""

__doc__ = "Extract example programs."


import argparse
import re
from pathlib import Path
from typing import Optional

ROOT_PATH = Path(__file__).resolve().parent.parent
DEFAULT_EXAMPLE_PATH = ROOT_PATH / "example"


def process_file(source_file: Path, output_dir: Path) -> None:
    """Process a file."""
    lines = source_file.read_text().splitlines()

    NONE = 0  # noqa: N806
    CODE = 1  # noqa: N806
    PROGRAM = 2  # noqa: N806

    mode = NONE
    suffix = ""
    program_name = ""
    program_start = -1

    for lineno, line in enumerate(lines):
        if mode == NONE:
            if line.startswith("```fortran"):
                mode = CODE
        elif mode == CODE:
            if not line.strip():
                continue
            m = re.match(r"^\s*program\s+(demo\w*|example\w*)\s*$", line, re.IGNORECASE)
            if m:
                mode = PROGRAM
                program_name = m.group(1).lower()
                program_start = lineno
                suffix = ".f" if m.group(0).startswith(" " * 6) else ".f90"
            else:
                mode = NONE
        elif mode == PROGRAM:
            if line.startswith("```"):
                (output_dir / f"{program_name}{suffix}").write_text(
                    "\n".join(lines[program_start:lineno]) + "\n"
                )
                mode = NONE


def main() -> None:
    """Entry point."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "files", help="source files to be processed", type=Path, nargs="*"
    )
    parser.add_argument("--dir", help="output directory", type=Path)
    args = parser.parse_args()

    source_files = args.files
    if not source_files:
        # When no files are given, search for all Markdown files.
        source_files = tuple(ROOT_PATH.glob("*.md"))

    output_dir: Optional[Path] = args.dir

    if not output_dir:
        output_dir = DEFAULT_EXAMPLE_PATH

    for f in source_files:
        process_file(f, output_dir)


if __name__ == "__main__":
    main()
