# Changelog

## rjd3toolkit 3.6.0

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.1.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

### [Unreleased](https://github.com/rjdverse/rjd3toolkit/compare/v3.6.0...HEAD)

### [3.6.0](https://github.com/rjdverse/rjd3toolkit/compare/v3.5.1...v3.6.0) - 2025-11-21

#### Changed

- default group value with mts objects in modelling_context
  [\#107](https://github.com/rjdverse/rjd3toolkit/issues/107)
- Examples are executed only if Java version \>= 17

#### Removed

- `fct` parameter in
  [`set_automodel()`](https://rjdverse.github.io/rjd3toolkit/reference/set_automodel.md)
  [\#85](https://github.com/rjdverse/rjd3toolkit/issues/85)

#### Added

- [`get_java_version()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  function to compute the Java installed version
- `current_java_version` character string with the current installed
  Java version
- `minimal_java_version` character string with the minimum viable Java
  version for rjdverse
- [`get_date_min()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  to get the minimum date
- [`get_date_max()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  to get the maximum date
- New example to use the functions
  ([\#85](https://github.com/rjdverse/rjd3toolkit/issues/85))
- Documentation of `bias` argument in
  [`set_benchmarking()`](https://rjdverse.github.io/rjd3toolkit/reference/set_benchmarking.md)
- New datasets : `Electricity` (French national electricity consumtion),
  `Births` (Number of births registered in France from 1968 to 2024),
  `x13_spec_default`(Default X13 specification) and
  `tramoseats_spec_default` (Default Tramo-Seats specification)

### [3.5.1](https://github.com/rjdverse/rjd3toolkit/compare/v3.5.0...v3.5.1) - 2025-06-18

#### Changed

- New JARS

### [3.5.0](https://github.com/rjdverse/rjd3toolkit/compare/v3.3.0...v3.5.0) - 2025-04-09

#### Changed

- New JARS
- Replace `.` with `_` in function’s name
  [\#88](https://github.com/rjdverse/rjd3toolkit/issues/88)
- The dataset `retail` is renamed `Retail`
  [\#89](https://github.com/rjdverse/rjd3toolkit/issues/89)
- The number of regressor was false for TD3c
  [\#92](https://github.com/rjdverse/rjd3toolkit/issues/92)

#### Added

- Spline functions (periodic, b-splines, cardinal splines)
- function `.add_ud_var` (from {rjd3tramoseats} and {rjd3x13})
- warning added in the function `calendar_td`
  [\#10](https://github.com/rjdverse/rjd3toolkit/issues/10)
- Regressor `TD2c` for regarima specifications
  [\#53](https://github.com/rjdverse/rjd3toolkit/issues/53)

### [3.3.0](https://github.com/rjdverse/rjd3toolkit/compare/v3.2.4...v3.3.0) - 2024-10-28

#### Changed

- New JARS
- Improve Canova-Hansen tests for seasonality and trading days (new
  options, more output)
- Document (UC)ARIMA models

### [3.2.4](https://github.com/rjdverse/rjd3toolkit/compare/v3.2.2...v3.2.4) - 2024-07-12

#### Changed

- New .jar (related to release
  [3.2.4](https://github.com/jdemetra/jdplus-main/releases/tag/v3.2.4))
- Some linting of R functions

#### Fixed

- Correct SA decomposition with backcasts and forecasts (Java to R
  transfer) [\#2](https://github.com/rjdverse/rjd3tramoseats/issues/2)

### [3.2.2](https://github.com/rjdverse/rjd3toolkit/compare/v3.2.1...v3.2.2) - 2024-03-15

### [3.2.1](https://github.com/rjdverse/rjd3toolkit/compare/v3.2.0...v3.2.1) - 2023-12-12

### [3.2.0](https://github.com/rjdverse/rjd3toolkit/compare/v3.1.0...v3.2.0) - 2023-11-24

### [3.1.0](https://github.com/rjdverse/rjd3toolkit/compare/v3.0.0...v3.1.0) - 2023-10-11

### [3.0.0](https://github.com/rjdverse/rjd3toolkit/releases/tag/v3.0.0) - 2023-06-14

#### Added

- Release based on JD+\_main : v3.0.2
