# Malef
**Malef** is a simple cross-compatible toolkit for writing terminal-based
applications in Ada 2022. The library is completely written in Ada and has no
dependencies.

This project's objective is to provide a simple and intuitive way of writting
blazing fast Terminal User Interfaces (TUI). There is a lot of effort put into
making API convenient and intuitive so the the developer doesn't neet to think
about low-level stuff.

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![License: FDL 1.3](https://img.shields.io/badge/License-FDL%20v1.3-blue.svg)](http://www.gnu.org/licenses/fdl-1.3)

**The library is in active development. The API is not definitive yet but it is
usable**

## Description
**Malef** is an open source and free library written in Ada to create Terminal
based User Interfaces. It divided in two parts: the _Drawing Toolkit_ and the
_Widget Toolkit_. The _Drawing Toolkit_ provides the low-level stuff: mainly
drawing and composition primitives. And the _Widget Toolkit_ abstracts the way
of building Applications using an interface similar to other G.U.I. toolkits
such as Gtk or Qt using _widgets_.

### The _Drawing Toolkit_
The _Drawing Toolkit_ provides a low-level API for drawing Surfaces,
composing them in _Groups_ and showing them on the _Window_. If you want to
create your own Widgets you will have to use this toolkit. See the packages
under [src/drawing](src/drawing).

#### Surfaces
A `Surface` is just a 2-dimensional Matrix of Glyphs (`Wide_Wide_Character`s),
Colours and Styles. Drawing on them is very simple, you can take a look at
the `malef-surfaces.ads` package to see all the available functions to draw.
The functions added to that package are for convenience.

Also, some types have their own String Literals (new in Ada 2022). So you can
specify a colour with an array or with an HTML colour tags.

```ada
with Malef.Surfaces;

procedure Surface_Example is
   My_Surface : Malef.Surfaces.Surface (3, 12);   -- Specify the number of
                                                  -- Rows and Columns.

begin

   My_Surface (1, 1) := 'H';
   My_Surface.Put (1, 2, "ello, World");
   My_Surface.Fill_Background ("#507878");
   My_Surface.Fill_Foreground ((1, 1), (1, 12), "#CCC");
   My_Surface.Fill ((1, 1), (1, 5), (Bold => True, others => False));

end Surface_Example;
```

Surfaces may have transparency, colours are represented as RGBA values (Red,
Green, Blue and Alpha), where _Alpha_ is the opacity of the colour (255 is
completely opaque and 0 is completely transparent).


#### Groups
A `Group` is a data-structure that may contain different `Surface`s or even
other `Group`s. It is similar to the _Groups_ of Layers you can create when
you work with GIMP.

```

                  _________________
Bottom Layer .   /,,,,,,,,,,,,,,,,/| <----------- Top Layer
             |  /________________/ |
             V _|_|___________   | |
              /.|.|......... /|  | |
         ____/..|.|........ /_|__|_|______
        /   /...|......... /  |  | |     /
       /   /....|........ /___|  | |    /
      /   /______________/ .. /  | |   /
     /    |   /.|........|.../___|_|  /
    /     |  /..|;;;;;;;;|;;,,,,,|,/ /    <----- Group Base Layer
   /      | /............|.______|/ /
  /       |/_____________|/        /
 /                                /
/________________________________/
```

Each Layer has a opacity associated with it and a `Layer_Mode` (the function
to be used in order to mix a layer with the layer below).

Groups are aggregates you can define your groups just like:

```ada
with Malef.Surfaces; use Malef.Surfaces;
with Malef.Groups;   use Malef.Groups;

procedure Group_Example is
   Red      : Surface (10, 10);
   My_Group : Group (6) := [Layer (Red, Opacity => 0.5),  -- Opacity
                            Layer (Red, Hidden => True),  -- Hidden?
                            Layer (Red, (2, 2)),          -- Position
                            No_Layer,                     -- Leave free space
                            Layer (20, 10),               -- Create a surface
                                                          -- of a given size.
                            Layer ([Layer (Red),          -- Add a group
                                    Layer (Red),
                                    Layer (Red)]
                                    (10, 20),
                                    Opacity => 0.9)];
   -- When adding a Surface to a Group a copy is always made, so changing the
   -- value of the `Red` surface won't change the contents of the group.
   -- Instead, use the `renames` directive to take a reference of the surface.
   -- Tampering rules will be running while you have a reference, so the group
   -- cannot be modified while it is being referenced.
   My_Surface renames Group (1).Set_Surface.Element;     -- Can be modified
   Surface_View renames Group (1).Get_Surface.Element;   -- Just a view

begin

   My_Surface.Fill_Background ("#F00");
   Red.Fill_Background ("#F00");
   My_Group.Insert (2, Red);  -- Error: Tampering because we have a reference.
                              -- If we didn't have a reference, it would be
                              -- correct.

end Group_Example;
```

Adding a new Layer to a group implies a copy. If you are adding a group to
another group a deep copy is made. And it may kill performance. Instead use the
Move function, it is similar to move semantics in C++. It takes ownership of
the group and clears the other group so it can be _moved_ inside another group.

```ada
with Malef.Surfaces; use Malef.Surfaces;
with Malef.Groups;   use Malef.Groups;

procedure Move_A_Group_Example is
   Red, Green, Blue : Surface (10, 10);
   Group_A : Group (3) := [Layer (Red), Layer (Green), Layer (Blue)];
   Group_B : Group (4) := [Layer (Red), Layer (Group_A), Layer (Green),
                           Layer (Blue)];    -- Group_A is copied
   Group_C : Group (4) := [Layer (Red), Move (Group_A), Layer (Green),
                           Layer (Blue)];    -- Group_A is moved and can no
                                             -- longer be used.
   Group_D : Group (2) := [Layer (Blue),
                           Layer ([Red, Green])]; -- The second group is copied
   Group_E : Group (2) := [Layer (Blue),
                           Move (Layer([Red, Green]))];  -- No copies

begin
   null;
end Move_A_Group_Example;

```

#### Window
The `Window` is a `protected` object that contains a single `Group`. You can
assign _callbacks_ to the `Window` using a _subscription/notifier_ model, i.e.,
when an event occurs, all subscribers are notified of that event. This will be
done in different tasks to avoid blocking the `Window` itself.

Let's see the first working example.
```ada
with Malef.Groups;
with Malef.System;
with Malef.Window;

procedure RGB_Window is
   use Malef.Groups;
   My_Group : Group := [Layer (10, 10, (1, 1), Opacity => 0.3),
                        Layer (10, 10, (1, 5), Opacity => 0.3),
                        Layer (10, 10, (5, 3), Opacity => 0.3)];
   Red   renames My_Group.Set_Surface (1).Element;
   Green renames My_Group.Set_Surface (2).Element;
   Blue  renames My_Group.Set_Surface (3).Element;
begin

   Malef.System.Initialize;      -- Initialize the subsystem

   Red.Fill_Background ("#FF0000");
   Green.Fill_Background ("#00FF00");
   Blue.Fill_Background ("#0000FF");

   Malef.Window.Window.Set_Group (My_Group);
   -- If you want to process the group as it is in a protected object you have
   -- to pass a function to the `Process_Group` procedure.
   Malef.Window.Window.Display;

   Malef.System.Finalize;        -- Finalize the subsystem

end RGB_Window;
```

#### Callbacks

#### System
The `Malef.System` package contains initialisation, finalisation and other
functions about the underlying system. If the developer forgets to finalize the
library or even if and exception is thrown, this packages makes sure to clean
up the terminal.

### The Widget Toolkit
**TODO**

You can see examples in the [examples directory](examples/).

## Compilation

| System  |                             Status                             |
|:-------:|:--------------------------------------------------------------:|
|  Linux  |  ![Linux](https://img.shields.io/badge/build-passing-success)  |
| Windows | ![Windows](https://img.shields.io/badge/build-passing-success) |

| Subsystem |                                  Status                                   |
|:---------:|:-------------------------------------------------------------------------:|
|   ANSI    |        ![ANSI](https://img.shields.io/badge/build-passing-success)        |
|    CMD    |        ![CMD](https://img.shields.io/badge/build-passing-success)         |
| Terminfo  | ![Terminfo](https://img.shields.io/badge/build-not_implemented-important) |

_There are some problems with colours in the CMD subsystem, everything else
works though. But the ANSI subsystem is still more robust._

## Tests
**TODO**

| System  |                                  Status                                  |
|:-------:|:------------------------------------------------------------------------:|
|  Linux  |  ![Linux](https://img.shields.io/badge/tests-not_implemented-important)  |
| Windows | ![Windows](https://img.shields.io/badge/tests-not_implemented-important) |

## How to build it?

**IMPORTANT**: It only compiles with GCC >=13.2.0

Malef uses [Alire](https://alire.ada.dev/), you only have to do

> alr with malef

On a project and you are ready to go. (If have to publish it first)

You can clone this repository and compile the examples:

> cd examples

> alr build

All examples will appear in the `bin` directory.

## Licenses
This library is made available under the [GPLv3](LICENSE) license.
