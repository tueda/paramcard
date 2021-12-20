#!/bin/sh
#
# A Fortran linter.
#
# This program tries to detect:
# - blank lines between procedures
#   https://github.com/pseewald/fprettify/issues/116
# - double quotes
#   https://github.com/pseewald/fprettify/issues/118
# - trailing semicolons
#   https://github.com/pseewald/fprettify/issues/120
#
# Requirements:
#   python >= 3.6.
#
""":" .
exec python3 "$0" "$@"
"""
import re
import sys
from pathlib import Path
from typing import Any, Sequence

__doc__ = "A Fortran linter."


EXCLUDE_PATH = ("api-doc", "build")
DEFAULT_EXTS = (".f", ".F", ".f90", ".F90", ".fypp")

failed = False  # TODO: don't use global.


def error_line(
    filename: str, lineno: int, start: int, end: int, line: str, message: str
) -> None:
    global failed

    INDENT = "    "
    SPACE = " "
    MARK = "^"
    LEFT_ELLIPSIS = "... "
    RIGHT_ELLIPSIS = " ..."

    MAX_LINE = 79 - len(INDENT) - len(LEFT_ELLIPSIS) - len(RIGHT_ELLIPSIS)
    LEFT_MARGIN = 15

    failed = True

    print(f"{filename}:{lineno + 1}:{start + 1}: {message}")

    if len(line) > MAX_LINE:
        skip = max(start - LEFT_MARGIN, 0)
        if skip > 0:
            line = LEFT_ELLIPSIS + line[skip:]
            start -= skip - len(LEFT_ELLIPSIS)
            end -= skip - len(LEFT_ELLIPSIS)
        if len(line) > MAX_LINE:
            line = line[:MAX_LINE] + RIGHT_ELLIPSIS
            end = min(end, MAX_LINE)

    print(INDENT + line)
    print(INDENT + SPACE * start + MARK * (end - start))


def process_line(filename: str, lineno: int, line: str) -> None:
    """Process a line."""
    # NOTE: this is very far from perfect.

    orig_line = line

    # Fortran 90 comment.
    if re.match(r"^\s*!", line):
        return

    # Presumably FORTRAN 77 comment.
    if re.match(r"^[cC]", line):
        return

    # cpp line.
    if re.match(r"^\s*#(?!:)", line):
        return

    def replace_single_quotes(m: Any) -> str:
        s = m.group(0).replace('"', "_").replace("!", "_").replace("#", "_")
        return s  # type: ignore

    # Handle single quotes.
    line = re.sub(r"'[^']*?'", replace_single_quotes, line)

    # Handle line comments.
    if re.match(r"^\s*#:", line):
        # fypp line.
        line = line.replace("#", " ", 1)
        line = re.sub("#.*", "", line)
    else:
        # Fortran line.
        line = re.sub("!.*", "", line)

    # Check double quotes.
    m = re.search(r"\"[^']*\"", line)
    if m:
        error_line(
            filename, lineno, m.start(0), m.end(0), orig_line, "double quotes found"
        )

    # Check trailing semicolons.
    m = re.search(r"(;)\s*$", line)
    if m:
        error_line(
            filename, lineno, m.start(1), m.end(1), orig_line, "double quotes found"
        )


def check_vertical_blanks(filename: str, lines: Sequence[str]) -> None:
    """Check vertical blank lines in the source."""
    NONE = 0
    BEGIN = 1
    END = 2

    previous = NONE
    n_blanks = 0

    for lineno, line in enumerate(lines):
        if re.match(r"^\s*!", line):
            current = NONE
        else:
            m = re.match(r"^\s*(?:(?:pure|recursive)\s*)*(subroutine|function)\b", line)
            if m:
                current = BEGIN
            else:
                m = re.match(r"^\s*end\s*(?:subroutine|function)\b", line)
                if m:
                    current = END
                else:
                    current = NONE

        if current == BEGIN and previous == END:
            if not m:
                raise AssertionError
            error_line(
                filename,
                lineno,
                m.start(1),
                m.end(1),
                line,
                f"blank line needed before beginning {m.group(1)}",
            )

        previous = current

        if re.match(r"^\s*$", line):
            n_blanks += 1
        else:
            n_blanks = 0

        if n_blanks == 2:
            error_line(
                filename, lineno, 0, max(len(line), 1), line, f"too many blank lines"
            )


def process_file(path: Path) -> None:
    """Process a file."""
    lines = path.read_text().splitlines()
    filename = str(path)
    for lineno, line in enumerate(lines):
        # NOTE: limitation: linewise.
        process_line(filename, lineno, line)
    check_vertical_blanks(filename, lines)


def main() -> None:
    """Entry point."""

    source_files = sys.argv[1:]

    if not source_files:
        source_files = []
        for ext in DEFAULT_EXTS:
            source_files.extend(str(f) for f in Path(".").glob(f"**/*{ext}"))
        source_files = [
            f
            for f in source_files
            if all(not f.startswith(excluded) for excluded in EXCLUDE_PATH)
        ]

    for f in source_files:
        process_file(Path(f))

    if failed:
        exit(1)


if __name__ == "__main__":
    main()
