# Malef
Malef is a terminal/console-handling library written in Ada.

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Description
Malef is an open source and free library written in Ada (with some parts
written in C) that works similarly to the Ncurses library but with a new data
type called _Surfaces_.

## Bindings
The Malef library will have bindings for other programming languages like C or
Python3 in the future, maybe before the first version release.

## Compilation

| System  | Status |
|:-------:|:------:|
| Linux   | ![Linux](https://img.shields.io/badge/build-passing-success)     |
| Windows | ![Windows](https://img.shields.io/badge/build-failing-critical)  |
| Unix    | ![Unix](https://img.shields.io/badge/build-not_tested-important) |

| Subsystem | Status |
|:---------:|:------:|
| ANSI      | ![ANSI](https://img.shields.io/badge/build-passing-success) |
| CMD       | ![CMD](https://img.shields.io/badge/build-failing-critical) |


## Tests

| System  | Status |
|:-------:|:------:|
| Linux   | ![Linux](https://img.shields.io/badge/tests-passing-success)    |
| Windows | ![Windows](https://img.shields.io/badge/tests-failing-critical) |

| Language | Status |
|:--------:|:------:|
| Ada      | ![Ada](https://img.shields.io/badge/tests-passing-success)     |
| C        | ![C](https://img.shields.io/badge/tests-passing-success)       |
| Python3  | ![Python3](https://img.shields.io/badge/tests-passing-success) |

## Versions
This are the milestones that I want to achieve for each of the versions. Every
version will be able to run in future versions perfectly. Every version will
have a codename yet to decide. Also many of the milestones may change during
the development of the project mainly the furthest ones.

### Version 0.0
This is the begining of the development the API won't be compleated yet.

### Version 1.0
The first version will have everything the library aims to do:

 * Surfaces
 * Sprites (Sprites are a set of surfaces)
 * Colours and Styles
 * Input/Output
 * Simple event handling
 * Bindings for C and Python3
 * Cursors
 * Support for ANSI-compliant terminals and Windows CMD.

### Version 2.0
The versions after the version 1.0 will try to fix bugs and expand the library
with an optional API. That optional API will be expanded during the version
just before i.e. version 2.0's API will be written during versions 1.X at the
same time the bugs from 1.0 are fixed. But it won't be compiled by default,
only for debugging and development.

The second version will add some predefined structures.

 * Menus
 * Scrolling
 * Buttons
 * Fields (Fields are like: insert_text_hear, choose_a_year...)
 * And more

### Version 3.0
This version will try to add real cursor and keyboard input and maybe guess the
keyboard layout.

 * Keyboard Layout
 * Keyboard Input
 * Mouse Input
 * Image2ASCII converter
 * Video2ASCII converter

### Version 4.0
This version will try to make the library available to all kinds of terminals
and consoles and test it in all of them. Also it will try to add embedable
terminals for Web development and a new terminal emulator which will be enhaced
for Malef developing.

 * Compatibility
 * WebAssembly
 * Terminal Emulator

### Not planned yet
There are some features that may or may not be available in the future, but
they are just ideas:

 * Audio
 * A kind of stream that converts the ANSI escape sequences written into standard output into the required system calls.
 * Compatibility with Ncurses, so a library written for Ncurses can be linked to Malef and viceversa.


## How to build it?
Better instructions will be given in the future.


## Licenses
This library is made available under the [GPLv3](LICENSE) license.
