# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres
to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased]

### Changed

* New JARS
* Replace `.` with `_` in function's name #88
* The dataset `retail` is renamed `Retail` #89
* The number of regressor was false for TD3c #92

### Added

* Spline functions (periodic, b-splines, cardinal splines)
* function `.add_ud_var` (from {rjd3tramoseats} and {rjd3x13})
* warning added in the function `calendar_td` #10
* Regressor `TD2c` for regarima specifications #53


## [3.3.0] - 2024-10-28

### Changed

* New JARS
* Improve Canova-Hansen tests for seasonality and trading days (new options, more output)
* Document (UC)ARIMA models


## [3.2.4] - 2024-07-12

### Changed

* New .jar (related to release [3.2.4](https://github.com/jdemetra/jdplus-main/releases/tag/v3.2.4))
* Some linting of R functions

### Fixed

- Correct SA decomposition with backcasts and forecasts (Java to R transfer) [#2](https://github.com/rjdverse/rjd3tramoseats/issues/2)


## [3.2.2] - 2024-03-15


## [3.2.1] - 2023-12-12


## [3.2.0] - 2023-11-24


## [3.1.0] - 2023-10-11


## [3.0.0] - 2023-06-14

### Added

* Release based on JD+_main : v3.0.2


[Unreleased]: https://github.com/rjdverse/rjd3toolkit/compare/v3.3.0...HEAD
[3.3.0]: https://github.com/rjdverse/rjd3toolkit/compare/v3.2.4...v3.3.0
[3.2.4]: https://github.com/rjdverse/rjd3toolkit/compare/v3.2.2...v3.2.4
[3.2.2]: https://github.com/rjdverse/rjd3toolkit/compare/v3.2.1...v3.2.2
[3.2.1]: https://github.com/rjdverse/rjd3toolkit/compare/v3.2.0...v3.2.1
[3.2.0]: https://github.com/rjdverse/rjd3toolkit/compare/v3.1.0...v3.2.0
[3.1.0]: https://github.com/rjdverse/rjd3toolkit/compare/v3.0.0...v3.1.0
[3.0.0]: https://github.com/rjdverse/rjd3toolkit/releases/tag/v3.0.0
