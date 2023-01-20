# Changelog

<a name="0.2.2"></a>
## [0.2.2] (2023-01-20)

### Fixed
- Remove GNU Fortran warning "Legacy Extension: Comma before i/o item list".
  ([509ebd1](https://github.com/tueda/paramcard/commit/509ebd1bc01e8331c6a816c2338ee2d95b9d0dbe))


<a name="0.2.1"></a>
## [0.2.1] (2022-03-15)

### Fixed
- Forgotten library initialization in `paramcard_set` and `paramcard_summary`.
  ([#3](https://github.com/tueda/paramcard/issues/3), [#4](https://github.com/tueda/paramcard/issues/4))


<a name="0.2.0"></a>
## [0.2.0] (2022-02-16)

### BREAKING CHANGE

- Public procedures in the `paramcard` module have been renamed
  in such a way that they have the common prefix `paramcard_`.
  ([#2](https://github.com/tueda/paramcard/issues/2))

### Added

- `paramcard_output` procedure,
  which allows the end user to easily customize the output.
  ([bae2725](https://github.com/tueda/paramcard/commit/bae272584fc2ee3104a7226894285dcf3f31017b))


<a name="0.1.0"></a>
## 0.1.0 (2021-12-30)

- First version.


[0.2.2]: https://github.com/tueda/paramcard/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/tueda/paramcard/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/tueda/paramcard/compare/v0.1.0...v0.2.0
