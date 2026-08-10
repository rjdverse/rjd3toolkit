# Changelog

## rjd3toolkit 3.8.0.9000

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.1.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

### [Unreleased](https://github.com/rjdverse/rjd3toolkit/compare/v3.8.0...HEAD)

#### Added

- New function `complete_modelling_context` to add new regressors and
  variables to a modelling_context object

#### Deprecated

- `sa_preprocessing` is now deprecated. Please don’t use anymore this
  function. [\#122](https://github.com/rjdverse/rjd3toolkit/issues/122)

#### Changed

- `modelling_context` accepts regressor with and without names in nested
  structure.
- `add_usrdefvar` accepts multiple regressor in the same group.
  [\#106](https://github.com/rjdverse/rjd3toolkit/issues/106)
- `add_usrdefvar` renames automatically the group or variables that
  contains a dot (`.`)
  [\#148](https://github.com/rjdverse/rjd3toolkit/issues/148).

#### Fixed

- `modelling_context` now accept `JD3_TS`, `JD3_DYNAMICTS` and
  `JD3_TSCOLLECTION` object
  [\#111](https://github.com/rjdverse/rjd3toolkit/issues/111)
- All unamed variables are regrouped together under the `r` group.
  [\#107](https://github.com/rjdverse/rjd3toolkit/issues/107)
- Unamed regressors and regressors with duplicated names are renamed
  with `x1`, `x2`… pattern.
  [\#107](https://github.com/rjdverse/rjd3toolkit/issues/107)
- Variables and regressor with dot (`.`) in their name are renamed in
  `modelling_context`. The dot is replaced with an underscore.
  [\#148](https://github.com/rjdverse/rjd3toolkit/issues/148)

### [3.8.0](https://github.com/rjdverse/rjd3toolkit/compare/v3.7.1...v3.8.0) - 2026-07-09

#### Changed

- Updated JARS from `jdplus-main` to
  [3.8.0](https://github.com/jdemetra/jdplus-main/releases/tag/v3.8.0)

#### Fixed

- Fix bug in weighted calendar regressors.
  [rjdverse/rjd3workspace#131](https://github.com/rjdverse/rjd3workspace/issues/131)

### [3.7.1](https://github.com/rjdverse/rjd3toolkit/compare/v3.6.0...v3.7.1) - 2026-03-10

#### Fixed

- Bug with
  [`to_tscollection()`](https://rjdverse.github.io/rjd3toolkit/reference/to_tscollection.md)
  (moniker conversion)

#### Changed

- Use `get_java_version()` instead of `current_java_version`
- New JARS related to version
  [3.7.1](https://github.com/jdemetra/jdplus-main/releases/tag/v3.7.1)

### [3.6.0](https://github.com/rjdverse/rjd3toolkit/compare/v3.5.1...v3.6.0) - 2025-11-21

#### Changed

- default group value with mts objects in modelling_context
  [\#107](https://github.com/rjdverse/rjd3toolkit/issues/107)
- Examples are executed only if Java version \>= 21
- New JARS related to version
  [3.6.0](https://github.com/jdemetra/jdplus-main/releases/tag/v3.6.0)

#### Removed

- `fct` parameter in
  [`set_automodel()`](https://rjdverse.github.io/rjd3toolkit/reference/set_automodel.md)
  [\#85](https://github.com/rjdverse/rjd3toolkit/issues/85)

#### Added

- `get_java_version()` function to compute the Java installed version
- `current_java_version` character string with the current installed
  Java version
- `minimal_java_version` character string with the minimum viable Java
  version for rjdverse
- [`get_date_min()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  to get the minimum date
- [`get_date_max()`](https://rjdverse.github.io/rjd3toolkit/reference/jd3_utilities.md)
  to get the maximum date
- New example to use the functions
  [\#85](https://github.com/rjdverse/rjd3toolkit/issues/85)
- Documentation of `bias` argument in
  [`set_benchmarking()`](https://rjdverse.github.io/rjd3toolkit/reference/set_benchmarking.md)
- New datasets : `Electricity` (French national electricity
  consumption), `Births` (Number of births registered in France from
  1968 to 2024), `x13_spec_default`(Default X13 specification) and
  `tramoseats_spec_default` (Default Tramo-Seats specification)
- New [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) method for
  `JD3_DICTIONARY` and `JD3_FULL_DICTIONARY`

### [3.5.1](https://github.com/rjdverse/rjd3toolkit/compare/v3.5.0...v3.5.1) - 2025-06-18

#### Changed

- New JARS related to version
  [3.5.1](https://github.com/jdemetra/jdplus-main/releases/tag/v3.5.1)

### [3.5.0](https://github.com/rjdverse/rjd3toolkit/compare/v3.3.0...v3.5.0) - 2025-04-09

#### Changed

- New JARS related to version
  [3.5.0](https://github.com/jdemetra/jdplus-main/releases/tag/v3.5.0)
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

- New JARS related to version
  [3.3.0](https://github.com/jdemetra/jdplus-main/releases/tag/v3.3.0)
- Improve Canova-Hansen tests for seasonality and trading days (new
  options, more output)
- Document (UC)ARIMA models

### [3.2.4](https://github.com/rjdverse/rjd3toolkit/compare/v3.2.2...v3.2.4) - 2024-07-12

#### Changed

- New JARS related to version
  [3.2.4](https://github.com/jdemetra/jdplus-main/releases/tag/v3.2.4)
- Some linting of R functions

#### Fixed

- Correct SA decomposition with backcasts and forecasts (Java to R
  transfer) [\#2](https://github.com/rjdverse/rjd3tramoseats/issues/2)

### [3.2.2](https://github.com/rjdverse/rjd3toolkit/compare/v3.2.1...v3.2.2) - 2024-03-15

#### Changed

- New JARS related to version
  [3.2.2](https://github.com/jdemetra/jdplus-main/releases/tag/v3.2.2)

### [3.2.1](https://github.com/rjdverse/rjd3toolkit/compare/v3.2.0...v3.2.1) - 2023-12-12

#### Changed

- New JARS related to version
  [3.2.1](https://github.com/jdemetra/jdplus-main/releases/tag/v3.2.1)

### [3.2.0](https://github.com/rjdverse/rjd3toolkit/compare/v3.1.0...v3.2.0) - 2023-11-24

#### Changed

- New JARS related to version
  [3.2.0](https://github.com/jdemetra/jdplus-main/releases/tag/v3.2.0)

### [3.1.0](https://github.com/rjdverse/rjd3toolkit/compare/v3.0.0...v3.1.0) - 2023-10-11

#### Changed

- New JARS related to version
  [3.1.0](https://github.com/jdemetra/jdplus-main/releases/tag/v3.1.0)

### [3.0.0](https://github.com/rjdverse/rjd3toolkit/releases/tag/v3.0.0) - 2023-06-14

#### Added

- New JARS related to version
  [3.0.2](https://github.com/jdemetra/jdplus-main/releases/tag/v3.0.2)
