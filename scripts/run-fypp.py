#!/bin/sh
#
# Run fypp for preprocessing.
#
# Requirements:
#   python >= 3.6, fprettify >= 0.3.7, fypp >= 3.1.
#
""":" .
exec python3 "$0" "$@"
"""

__doc__ = "Run fypp for preprocessing."

import argparse
import os
import re
import shutil
import subprocess
from pathlib import Path
from typing import Optional


def guess_file(path: Path) -> str:
    """Guess the type of a file and return the corresponding file extension."""
    lines = path.read_text().splitlines()

    MAX_LINES = 500

    f77ish = True
    f90ish = False

    for i, line in enumerate(lines):
        if i >= MAX_LINES:
            break

        if f77ish:
            s = line[:5]
            if (
                not re.match("^[Cc*]", s)
                and not re.match("^[ \t]*[!#]", s)
                and re.match("[^ \t0-9]", s)
            ):
                f77ish = False

        if not f90ish:
            if not re.match("^[ \t]*!", line) and re.search(
                "program|module|subroutine|function", line, re.I
            ):
                f90ish = True

    if f77ish:
        return ".f"
    elif f90ish:
        return ".f90"
    else:
        raise RuntimeError(f"can't guess the file type: {path}")


def nfiles_str(nfiles: int) -> str:
    """Format `n files`."""
    if nfiles == 0:
        return "no files"
    elif nfiles == 1:
        return "1 file"
    else:
        return f"{nfiles} files"


def main() -> None:
    """Entry point."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "files", help="source files to be processed", type=Path, nargs="*"
    )
    args = parser.parse_args()

    source_files = args.files
    if not source_files:
        # When no files are given, recursively search for all .fypp files
        # from the project root.
        os.chdir(Path(__file__).resolve().parent.parent)
        source_files = list(Path(".").glob("**/*.fypp"))

    fypp_bin = shutil.which("fypp")
    if not fypp_bin:
        raise RuntimeError("fypp not found")

    fprettify_bin = shutil.which("fprettify")
    if not fprettify_bin:
        raise RuntimeError("fprettify not found")

    nchanged = 0

    for source_path in source_files:
        output_path = source_path.with_suffix(guess_file(source_path))
        if output_path.exists():
            old_output: Optional[str] = output_path.read_text()
        else:
            old_output = None

        subprocess.run([fypp_bin, source_path, output_path], check=True)
        if output_path.suffix == ".f90":
            subprocess.run([fprettify_bin, output_path], check=True)

        new_output = output_path.read_text()
        if old_output != new_output:
            print(f"{source_path} -> {output_path}")
            nchanged += 1

    nfiles = len(source_files)
    print(
        f"{nfiles_str(nfiles).capitalize()} processed. "
        f"{nfiles_str(nchanged).capitalize()} changed."
    )


if __name__ == "__main__":
    main()
