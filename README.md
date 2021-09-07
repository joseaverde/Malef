# Malef
Malef is a terminal/console-handling library written in Ada.

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![License: FDL 1.3](https://img.shields.io/badge/License-FDL%20v1.3-blue.svg)](http://www.gnu.org/licenses/fdl-1.3)

## Description
**Malef** is an open source and free library written in Ada (with some parts
written in C) to create Terminal User Interfaces with an approach similar to
graphical libraries. It will also provide bindings to the C and Python3
programming languages (they haven't been implemented yet).

You can see examples in the [examples directory](examples/).


## Bindings
The Malef library will have bindings for other programming languages like C or
Python3 in the future, a few tests have been done in early versions where they
perfectly work. I'm putting them aside for now until I have a strong library
core and API.

## Compilation

| System  | Status |
|:-------:|:------:|
| Linux   | ![Linux](https://img.shields.io/badge/build-passing-success)     |
| Windows | ![Windows](https://img.shields.io/badge/build-passing-success)   |
| Unix    | ![Unix](https://img.shields.io/badge/build-not_tested-important) |

| Subsystem | Status |
|:---------:|:------:|
| ANSI      | ![ANSI](https://img.shields.io/badge/build-passing-success) |
| CMD       | ![CMD](https://img.shields.io/badge/build-passing-success) |

_There are some problems with colours in the CMD subsystem, everything else
works though. But the ANSI subsystem is still more robust._

## Tests
The test directory hasn't been updated recently, in future versions new tests
will be added.

| System  | Status |
|:-------:|:------:|
| Linux   | ![Linux](https://img.shields.io/badge/tests-passing-success)    |
| Windows | ![Windows](https://img.shields.io/badge/tests-not_implemented-important) |

| Language | Status |
|:--------:|:------:|
| Ada      | ![Ada](https://img.shields.io/badge/tests-passing-success)     |
| C        | ![C](https://img.shields.io/badge/tests-not_implemented-critical)       |
| Python3  | ![Python3](https://img.shields.io/badge/tests-not_implemented-critical) |


## How to build it?
Better instructions will be given in the future. But basically download GNAT
and run
> gprbuild -p -Pmalef.gpr
With the options you want, a complete list can be seen in the
[shared.gpr](shared.gpr) source file.


## Licenses
This library is made available under the [GPLv3](LICENSE) license.
