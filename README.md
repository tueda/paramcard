# paramcard

[![Test](https://github.com/tueda/paramcard/actions/workflows/test.yml/badge.svg?branch=main)](https://github.com/tueda/paramcard/actions/workflows/test.yml?query=branch:main)
[![codecov](https://codecov.io/gh/tueda/paramcard/branch/main/graph/badge.svg)](https://codecov.io/gh/tueda/paramcard)

Fortran's command-line parameter input made simple.

This Fortran library provides a simple framework to handle input parameters
given by command-line arguments, like `./a.out a=1 dt=0.01 method=rk45`.
These parameters can also be read from specified input files as `./a.out input.txt`.


## Example

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
    call paramcard_get('x', x, 0.3_dp)
    call paramcard_get('y', y, 0.4_dp)
    call paramcard_get('msg', msg, '')
    call paramcard_summary

    print *, 'a + b = ', a + b
    print *, 'x + y = ', x + y
    if (msg /= '') print *, msg
end program demo
```
The `paramcard_get` subroutine takes 3 arguments: `name`, `variable` and `default_value`.
They specify the parameter name, the destination variable to store the parameter value
and the (optional) default value, respectively.
The `paramcard_summary` subroutine prints the summary of the input parameters and
checks if there are any given parameters that are actually unused in the program.
The compiled program will accept command-line arguments like
`./demo a=101 x=0.6 msg='Hello, World!'`, where parameters are overridden in the form of
`name=value`. Parameter names are case- and space-insensitive.
It is also possible to pass parameters via input text files like
`./demo input.txt`.
An example of an input file is as follows:
```ini
# Comment lines start with "#" or "!".
a = 101
x = 0.6
msg = Hello, World!
```
The output of the above example looks as follows:
```
a = 101 (default: 1)
b = 2
x = 0.59999999999999998 (default: 0.29999999999999999)
y = 0.40000000000000002
msg = Hello, World! (default: )
 a + b =          103
 x + y =    1.0000000000000000
 Hello, World!
```


## Getting started

The library works with Fortran 2008 compliant compilers.
Because this is a single-file library, one can just copy a MIT-licensed file
[`paramcard.f90`](https://raw.githubusercontent.com/tueda/paramcard/v0.2.3/src/paramcard.f90)
to one's project:
```bash
curl -O https://raw.githubusercontent.com/tueda/paramcard/v0.2.3/src/paramcard.f90
```
Alternatively, one can use this repository as a submodule of one's Git repository:
```bash
git submodule add https://github.com/tueda/paramcard.git extern/paramcard
git -C extern/paramcard checkout v0.2.3
```
which makes the library source available at `extern/paramcard/src/paramcard.f90`.

Integration with [`fpm`](https://github.com/fortran-lang/fpm) (v0.8.0+) is also available:
```toml
[dependencies]
paramcard = { git = "https://github.com/tueda/paramcard", tag = "v0.2.3" }
```

[CMake](https://cmake.org/) (v3.14+) integration with the [FetchContent](https://cmake.org/cmake/help/latest/module/FetchContent.html) module:
```cmake
include(FetchContent)
```
```cmake
FetchContent_Declare(
  paramcard
  GIT_REPOSITORY https://github.com/tueda/paramcard.git
  GIT_TAG v0.2.3
)
FetchContent_MakeAvailable(paramcard)
if(paramcard_POPULATED)
  add_library(paramcard STATIC ${paramcard_SOURCE_DIR}/src/paramcard.f90)
endif()
```
```cmake
target_link_libraries(main PRIVATE paramcard)
```

Integration with [`CPM.cmake`](https://github.com/cpm-cmake/CPM.cmake):
```cmake
CPMAddPackage(
  NAME paramcard
  GIT_REPOSITORY https://github.com/tueda/paramcard
  VERSION 0.2.3
  DOWNLOAD_ONLY YES
)
if (paramcard_ADDED)
  add_library(paramcard STATIC ${paramcard_SOURCE_DIR}/src/paramcard.f90)
endif()
```
```cmake
target_link_libraries(main PRIVATE paramcard)
```

## API

In the `paramcard` module, the following procedures are available.

### `subroutine paramcard_get(name, variable [, default_value])`
Read a parameter specified by `name` into `variable`.
```fortran
character(len=*), intent(in) :: name
character(len=:), intent(out), allocatable :: variable
character(len=*), intent(in), optional :: default_value
```
```fortran
character(len=*), intent(in) :: name
integer(kind=int8/int16/int32/int64), intent(out) :: variable
integer(kind=int8/int16/int32/int64), intent(in), optional :: default_value
```
```fortran
character(len=*), intent(in) :: name
real(kind=real32/real64), intent(out) :: variable
real(kind=real32/real64), intent(in), optional :: default_value
```
- `name`: the name of the parameter.
- `variable`: the variable to store the parameter value.
- `default_value`: the value to be used when the specified parameter is not defined from the input.
If the default value is not given, then an undefined parameter leads to an error.

### `subroutine paramcard_set(name, value [, consumed])`
Set (overwrite) a parameter specified by `name` as `value` for later use.
```fortran
character(len=*), intent(in) :: name
character(len=*), intent(in) :: value
logical, intent(in), optional :: consumed
```
```fortran
character(len=*), intent(in) :: name
integer(kind=int8/int16/int32/int64), intent(in) :: value
logical, intent(in), optional :: consumed
```
```fortran
character(len=*), intent(in) :: name
integer(kind=real32/real64), intent(in) :: value
logical, intent(in), optional :: consumed
```
- `name`: the name of the parameter.
- `value`: the value to be associated with the parameter.
- `consumed`: whether this parameter should be considered *consumed (used)* or not. (Default: `.true.`)

### `subroutine paramcard_parse(str)`
Set a parameter by parsing a string containing `NAME = VALUE`.
```fortran
character(len=*), intent(in) :: str
```
- `str`: the string to be parsed.

### `subroutine paramcard_summary([options...])`
Print the summary of input parameters.
```fortran
integer, optional, intent(in) :: unit
logical, optional, intent(in) :: only_changed
logical, optional, intent(in) :: show_default
logical, optional, intent(in) :: check_unused
character(len=*), optional, intent(in) :: prefix
```
- `unit`: the unit number for the output.
  (Default: `output_unit`)
- `only_changed`: print only parameters that are changed from those default values.
  (Default: `.false.`)
- `show_default`: print the default values.
  (Default: `.true.`)
- `check_unused`: check unused (not-*consumed*) parameters and raise an error if found.
  (Default: `.true.`)
- `prefix`: the prefix to each line in the output.
  (Default: `''`)

### `function paramcard_format(fmt)`
Perform a string formatting (interpolation) operation.
```fortran
character(len=*), intent(in) :: fmt
character(len=:), allocatable :: result
```
- `fmt`: the format to be used.
  Parameters are substituted by braces, for example, `'I have {n} apples.'`
- Return `result`: the formatted string.

### `subroutine paramcard_output([options...])`
Write output to a file with a format.
The output file name and the format are specified by parameters.
The output is constructed by using `paramcard_format`.
This subroutine does nothing if the file name or the format is empty.
```fortran
character(len=*), intent(in), optional :: file_param
character(len=*), intent(in), optional :: format_param
```
- `file_param`: the parameter to specify the output file.
  (Default: `'output_file'`)
- `format_param`: the parameter to specify the output format.
  (Default: `'output_format'`)


## Old-fashioned API

We also provide old-fashioned procedures (with some limitations) that can be used without using the module.

```fortran
      PROGRAM DEMO77
      INTEGER A, B
      DOUBLE PRECISION X, Y
      CHARACTER*40 MSG
C
      CALL PARAMCARD_GET_I('A', A, 1)
      CALL PARAMCARD_GET_I('B', B, 2)
      CALL PARAMCARD_GET_D('X', X, 0.3D0)
      CALL PARAMCARD_GET_D('Y', Y, 0.4D0)
      CALL PARAMCARD_GET_S('MSG', MSG, '')
      CALL PARAMCARD_SUMMARY
C
      WRITE (*,*) 'A + B = ', A + B
      WRITE (*,*) 'X + Y = ', X + Y
      IF (MSG .NE. '') WRITE (*,*) MSG
C
      STOP
      END
```

### `subroutine paramcard_get_s(name, variable, default_value)`
```fortran
character(len=*), intent(in) :: name
character(len=*), intent(out) :: variable
character(len=*), intent(in) :: default_value
```

### `subroutine paramcard_get_i(name, variable, default_value)`
```fortran
character(len=*), intent(in) :: name
integer, intent(out) :: variable
integer, intent(in) :: default_value
```

### `subroutine paramcard_get_r(name, variable, default_value)`
```fortran
character(len=*), intent(in) :: name
real, intent(out) :: variable
real, intent(in) :: default_value
```

### `subroutine paramcard_get_d(name, variable, default_value)`
```fortran
character(len=*), intent(in) :: name
double precision, intent(out) :: variable
double precision, intent(in) :: default_value
```

### `subroutine paramcard_set_s(name, value)`
```fortran
character(len=*), intent(in) :: name
character(len=*), intent(in) :: value
```

### `subroutine paramcard_set_i(name, value)`
```fortran
character(len=*), intent(in) :: name
integer, intent(in) :: value
```

### `subroutine paramcard_set_r(name, value)`
```fortran
character(len=*), intent(in) :: name
real, intent(in) :: value
```

### `subroutine paramcard_set_d(name, value)`
```fortran
character(len=*), intent(in) :: name
double precision, intent(in) :: value
```

### `subroutine paramcard_summary()`

### `subroutine paramcard_output(file_param, format_param)`
```fortran
character(len=*), intent(in) :: file_param
character(len=*), intent(in) :: format_param
```


## Development

```bash
brew install ford gcc git lcov pre-commit python fortran-lang/fortran/fpm
```

```bash
pre-commit install
```

```bash
pre-commit run --all-file               # linter, formatter and preprocessor
fpm test                                # testing
GCOV=gcov-12 ./scripts/gen-coverage.sh  # coverage report
ford API-doc-FORD-file.md               # documentation
```


## License

[MIT](https://github.com/tueda/paramcard/blob/main/LICENSE)
