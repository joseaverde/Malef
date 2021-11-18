# Malef
Malef is a terminal/console-handling library written in Ada.

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![License: FDL 1.3](https://img.shields.io/badge/License-FDL%20v1.3-blue.svg)](http://www.gnu.org/licenses/fdl-1.3)

## Description
**Malef** is an open source and free library written in Ada to create Terminal
User Interfaces with an approach similar to graphical libraries. It will also
provides bindings to the [C](https://github.com/joseaverde/CMalef) and
[Python3](https://github.com/joseaverde/PyMalef). programming languages.

You can see examples in the [examples directory](examples/).


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

## How to build it?
Better instructions will be given in the future. But basically download GNAT
and run
> gprbuild -p -Pmalef.gpr
With the options you want, a complete list can be seen in the
[shared.gpr](shared.gpr) source file.


## Licenses
This library is made available under the [GPLv3](LICENSE) license.
