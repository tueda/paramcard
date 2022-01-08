# paramcard

[![Test](https://github.com/tueda/paramcard/workflows/Test/badge.svg?branch=main)](https://github.com/tueda/paramcard/actions?query=branch:main)
[![codecov](https://codecov.io/gh/tueda/paramcard/branch/main/graph/badge.svg)](https://codecov.io/gh/tueda/paramcard)

Fortran's command-line parameter input made simple.

This Fortran library provides a simple framework to handle input parameters
given by command-line arguments, like `./a.out a=1 dt=0.01 method=rk45`.
These parameters can also be read from specified input files as `./a.out input.txt`.


## Usage

```fortran
program demo
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use paramcard, only: paramcard_get, paramcard_summary
    implicit none
    integer :: a, b
    real(dp) :: x, y
    character(:), allocatable :: msg

    call paramcard_get('a', a, 1)
    call paramcard_get('b', b, 2)
    call paramcard_get('x', x, 0.3d0)
    call paramcard_get('y', y, 0.4d0)
    call paramcard_get('msg', msg, '')
    call paramcard_summary

    print *, 'a + b = ', a + b
    print *, 'x + y = ', x + y
    if (msg /= '') print *, msg
end program demo
```
The `paramcard_get` procedure takes 3 arguments: `name`, `variable` and `default_value`.
They specify the parameter name, the destination variable to store the parameter value
and the (optional) default value, respectively.
The `paramcard_summary` procedure prints the summary of the input parameters and
aborts if there are any given parameters that are actually unused in the program.
The compiled program will accept command-line arguments like
`./demo a=101 x=0.6 msg=Hello`, where parameters are overridden in the form of
`name=value`. Parameter names are case- and space-insensitive.
It is also possible to pass parameters via input text files like
`./demo input.txt`.
An example of an input file is as follows:
```ini
# Comment lines start with "#" or "!".
a = 101
x = 0.6
msg = Hello
```


## Getting started

The library works with Fortran 2008 compliant compilers.
Because this is a single-file library, one can just copy a MIT-licensed file
[`paramcard.f90`](https://raw.githubusercontent.com/tueda/paramcard/v0.1.0/src/paramcard.f90)
to one's project:
```bash
curl -O https://raw.githubusercontent.com/tueda/paramcard/v0.1.0/src/paramcard.f90
```
Alternatively, one can use this repository as a submodule of one's Git repository:
```bash
git submodule add https://github.com/tueda/paramcard.git extern/paramcard
git -C extern/paramcard checkout v0.1.0
```
which makes the library source available at `extern/paramcard/src/paramcard.f90`.

Integration with [`fpm`](https://github.com/fortran-lang/fpm) is also available:
```toml
[dependencies]
paramcard = { git = "https://github.com/tueda/paramcard", tag = "v0.1.0" }
```

[CMake](https://cmake.org/) (v3.14+) integration with the [FetchContent](https://cmake.org/cmake/help/latest/module/FetchContent.html) module:
```cmake
include(FetchContent)
```
```cmake
FetchContent_Declare(
  paramcard
  GIT_REPOSITORY https://github.com/tueda/paramcard.git
  GIT_TAG        v0.1.0
)
FetchContent_MakeAvailable(paramcard)
if(paramcard_POPULATED)
  add_library(paramcard STATIC ${paramcard_SOURCE_DIR}/src/paramcard.f90)
endif()
```
```cmake
target_link_libraries(main PRIVATE paramcard)
```


## Development

```bash
brew install awvwgk/fpm/fpm ford gcc git lcov pre-commit python
```

```bash
pre-commit install
pre-commit install --hook-type commit-msg
```

```bash
pre-commit run --all-file               # linter, formatter and preprocessor
fpm test                                # testing
GCOV=gcov-11 ./scripts/gen-coverage.sh  # coverage report
ford API-doc-FORD-file.md               # documentation
```


License
-------

[MIT](https://github.com/tueda/paramcard/blob/main/LICENSE)
