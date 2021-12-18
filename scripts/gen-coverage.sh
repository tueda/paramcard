#!/bin/bash
#
# Generate a coverage report in the build directory.
#
# Usage:
#   gen-coverage.sh
#   GCOV=gcov-11 gen-coverage.sh
#
# Requirements:
#   fpm, gfortran, gcov, lcov or gcovr
#
set -eu
set -o pipefail

if command -v lcov >/dev/null; then
  lcov -z -d build
  fpm test --flag -coverage
  if [[ -z "${GCOV:-}" ]]; then
    lcov -c -d build -o build/coverage.info
  else
    lcov -c -d build -o build/coverage.info --gcov-tool $GCOV
  fi
  lcov -r build/coverage.info '*/build/dependencies/*' -o build/coverage.info
  lcov -r build/coverage.info '*/test/*' -o build/coverage.info
  genhtml -o build/coverage-lcov build/coverage.info
elif command -v gcovr >/dev/null; then
  find build -name '*.gcda' -exec rm -fv {} \;
  fpm test --flag -coverage
  mkdir -p build/coverage-gcovr
  # NOTE: gcovr uses $GCOV.
  gcovr -r . -f src --html --html-details -o build/coverage-gcovr/coverage.html
else
  echo 'error: neither lcov nor gcovr found' >&2
  exit 1
fi
