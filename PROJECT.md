PROJECT
=======

   This document will describe all the phases this project will try to do. The
*Project* will have reached it's first stable version with all the base
functionalities by verion **1.0**. Following versions will fix bugs and add
new extra functionalities like a Software Development Kit.


## Version 0.x
   This is the begining of the development of the API, many things might change
during the development of this version and it's unstable. It might not compile
for certain platforms.


## Version 1.x
   This version will have the base system of the library and bindings for both
Python3 and C. It will contain:

   * Surfaces
   * Boxes (Sprites are just a set of surfaces)
   * Colours
   * Styles
   * Input/Output
   * Simple Event Handling
   * Cursors
   * Support for both ANSI-compliant terminals and Windows CMD
   * Bell sound

   And it will try to improve the functionality of each of the components such
as:

   * Speed
   * Optimisations
   * Style-Replication (In case certain style is not available in the terminal)
   * Encoding Problems
   * Bug-fixes


## Version 2.x
   This version will try to add a simple Software Development Kit, and it will
be developed alongisde **version 1.x**. It will be an optional component of the
library, so it won't be compiled by default until version **v2.0.0** is
reached. It will try to add the following objects:

   * Menus
   * Scrolling
   * Buttons
   * Fields (Like in: insert-text-here, choose-a-date...)

   It will try to fix bugs, improve the speed and optimise it in thi version as
well. And fix any of the upcoming bugs. It will try to add a more functions to
control the terminal/console.


## Version 3.x
   This version will try to add a real cursor (the mouse) and improve the
keyboard input system without using *get_immediate*.

   * Keyboard Layout
   * Keyboard Input
   * Mouse Input
   * Encoding
   * Right to Left languages
   * Double width languages
   * Diacritics

## Version 4.x
   This version will try to make Malef as portable as possible, targetting
every system and allowing to compile a simple optimised terminal emulator
to move your programs anywhere.

   * Terminal Emulator (maybe with SDL-Ada, optimised with subsystems).
   * Widgets for Web pages using WebAssembly.
   * Tile loader in TTY.
   * (Maybe Audio)
   * A kind of pipe that takes the Stdout stream and converts the ANSI escape
   sequences into the required system calls.

