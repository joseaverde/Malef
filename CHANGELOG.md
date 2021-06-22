# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased]

## [0.3.0] - 2021-04-00
### Added
- Added database
- Added UTF-8 encoding
- Added "packager" for different package managers.

### Fixed
- Python binding now works on Windows
- Fixed UTF8 encoding bugs

## [0.2.4] - 2021-04-09
### Added
- Added Color type to Python binding.
- Added Palette type to Python binding.
- Added Enumeration Iterables to Python binding.
- Added Color Enumeration and Palette Enumeration types to Python binding.
### Fixed
- Fixed various bugs.
- Fixed all the Python binding.
- Fixed python iterators.

## [0.2.3] - 2021-02-05
### Added
- Added complete colour/palette support for C.
### Fixed
- Fixed Malef.h formatting.
- Fixed functions return value.

## [0.2.2] - 2021-01-30
### Added
- Added Surface type for Python3.
- Added test cases for Ada, C and Python3 and a test-engine written in AWK.
- Added and improved **dev-tools** to add functions to keep track of tasks to do and added more languages to [src-gen](dev-tools/src-gen.py)
### Fixed
- Fixed Surface type in C.
- Fixed formatting of C source files.

## [0.2.1] - 2021-01-06
### Fixed
- Fixed format in C header
- Fixed format in Python3 implementation

## [0.2.0] - 2021-01-05
### Added
- Added C binding for the functions declared in version **0.0.X**.
- Added *TEMPORARY* C binding for the functions declared in version **0.1.X**
- Added working (but unclean) binding of Python3 for the functions declared in version **0.0.X** but the Surface_Type.
- Added documentation for the C binding and C header file.
- Added more tests (all working).


## [0.1.2] - 2021-01-02
### Added
- Added documentation for the Malef.Colors package.

## [0.1.1] - 2021-01-02
### Added
- Added the Palette type.
- Added different palettes.
- Added system specific palettes.

### Fixed
- Fixed copyright notice in every source file.

## [0.1.0] - 2021-01-01
### Added
- Added Malef.Color API without documentation, yet (It will be bundled in the next patch).
- Added implementation for Malef.Color API.
- Added more exceptions

### Fixed
- Fixed date ranges in header files and some dev-tools.


## [0.0.1] - 2020-12-27
### Fixed
- Fixed Malef.Wrapper


## [0.0.0] - 2020-12-27
### Added
- This is the first (unreleased) version of Malef.
- Almost all types has been declared and defined.
- The resize event has been added.
- The controlled Surface_Type has been added.
- A wrapper function has been added.
- System specific implementations has been added for both Windows and Linux.

