# Roadmap
The library consists on the Toolkit for drawing surfaces, composing them in
boxes and manage them on windows. It abstracts the drawing on the screen for
every possible terminal and system.

Then it contains the Widget toolkit which can be used to create applications
like using Gtk or any other toolkit.

## Version 1.0

The Toolkit will implement:

* [ ] Surfaces (basic operations)
* [ ] Boxes (basic operations)
* [ ] Windows (basic operations)
* [ ] Subsystem Interface
 - [ ] Terminfo
 - [ ] CMD (for older Windows terminals without Terminfo)
 - [ ] ANSI (like Terminfo but optimised for ANSI-like terminals)
* [ ] Events
 - [ ] Resizing
 - [ ] Key Presses
 - [ ] Mouse

Then with the toolkit we will create Widgets:
https://docs.gtk.org/gtk4/visual_index.html
* [ ] Dialogs
* [ ] Grids
* [ ] Labels (alignment, direction and markup selector [Markdown, Plain])
* [ ] Buttons
* [ ] Progress Bars
* [ ] Separators
* [ ] Shadows
* [ ] Toolbars
* [ ] Check Buttons
* [ ] Radio Buttons
* [ ] Switches
* [ ] Drop_Down_Menus
* [ ] Text Entry
* [ ] Text Editor
* [ ] Frames

More Widgets will be Added as time passes.

## Version 2.0
Optimisations on inner Subsystems and internal structure for better
performance.

Add newer Widgets

## Version 3.0
Bundle executable with its own terminal and subsystem.
