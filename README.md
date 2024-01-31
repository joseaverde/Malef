# Malef
**Malef** is a simple cross-compatible toolkit for writing terminal-based
applications in Ada 2022. The library is completely written in Ada and has no
dependencies.

This project's objective is to provide simple way of writting Terminal User
Interfaces (T.U.I.) that are blazing fast. There is a lot of effort put into
making A.P.I. simple and convenient without the developer thinking about
low-level stuff.

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![License: FDL 1.3](https://img.shields.io/badge/License-FDL%20v1.3-blue.svg)](http://www.gnu.org/licenses/fdl-1.3)

## Description
**Malef** is an open source and free library written in Ada to create Terminal
User Interfaces. It divided in two parts: the _Drawing Toolkit_ and the
_Widget Toolkit_. The _Drawing Toolkit_ provides the lowlevel stuff: drawing
and composing primitives. And the _Widget Toolkit_ abstracts the way of
building Applications using an interface similar to other G.U.I. toolkits such
as Gtk or Qt.

### The _Drawing Toolkit_
The _Drawing Toolkit_ provides a lowlevel API for drawing Surfaces,
composing them in _Boxes_ and showing them on the _Window_. If you want to
create your own Widgets you will have to use this toolkit. See the packages
under [src/drawing](src/drawing).

#### Surfaces
A `Surface` is just a 2-dimensional Matrix of Glyphs (`Wide_Wide_Character`s),
Colours and Styles. Drawing on them is very simple, you can take a look at
the `malef-surfaces.ads` package to see all the available functions to draw.

Also, some types have their own String Literals (new in Ada 2022). So you can
specify a colour with an array or with an HTML tag.

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
other `Group`s. It is similar to _Groups_ of Layers you can create when working
with GIMP.

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

Each Layer has a opacity associated with it, also a `Layer_Mode` (the function
to be used in order to mix a layer with the layer bellow).

Examples: **TODO**

#### Window
The `Window` is a `protected` object that contains a single `Group`. You can
assign _callbacks_ to the `Window` using a _subscription/notifier_ model, i.e.,
when an event occurs, all subscribers are notified of that event. This will be
done in different tasks to avoid blocking the `Window` itself.

**TODO**

#### System
The `Malef.System` package contains initialisation, finalisation and other
functions about the underlying system. If the developer forgets to finalize the
library or even if and exception is thrown, this packages makes sure to clean
up the terminal.

### The Widget Toolkit
**TODO**

You can see examples in the [examples directory](examples/).

## Compilation

| System  | Status |
|:-------:|:------:|
| Linux   | ![Linux](https://img.shields.io/badge/build-passing-success)     |
| Windows | ![Windows](https://img.shields.io/badge/build-passing-success)   |

| Subsystem | Status |
|:---------:|:------:|
| ANSI      | ![ANSI](https://img.shields.io/badge/build-passing-success) |
| CMD       | ![CMD](https://img.shields.io/badge/build-passing-success) |
| Terminfo  | ![Terminfo](https://img.shields.io/badge/build-not_implemented-important) |

_There are some problems with colours in the CMD subsystem, everything else
works though. But the ANSI subsystem is still more robust._

## Tests
**TODO**

| System  | Status |
|:-------:|:------:|
| Linux   | ![Linux](https://img.shields.io/badge/tests-not_implemented-important)    |
| Windows | ![Windows](https://img.shields.io/badge/tests-not_implemented-important) |

## How to build it?
Malef uses [Alire](https://alire.ada.dev/), you only have to do

> alr with malef

On a project and you are ready to go. (If have to publish it first)

You can clone this repository and compile the examples:

> cd examples

> alr build

All examples will appear in the `bin` directory.

## Licenses
This library is made available under the [GPLv3](LICENSE) license.
