# paramcard

[![Test](https://github.com/tueda/paramcard/workflows/Test/badge.svg?branch=main)](https://github.com/tueda/paramcard/actions?query=branch:main)
[![codecov](https://codecov.io/gh/tueda/paramcard/branch/main/graph/badge.svg)](https://codecov.io/gh/tueda/paramcard)

Command-line parameter input made simple.


## Requirements

- Fortran 2008 compliant compiler


## Development

Requires [`python`](https://www.python.org/),
[`fpm`](https://github.com/fortran-lang/fpm),
[`ford`](https://github.com/Fortran-FOSS-Programmers/ford) and
[`pre-commit`](https://pre-commit.com/) (as well as
[`git`](https://git-scm.com/) and a Fortran compiler).

```bash
pre-commit install
pre-commit install --hook-type commit-msg

pre-commit run --all-file

fpm test
fpm run --example demo -- # + arguments
ford API-doc-FORD-file.md
```


License
-------

MIT
