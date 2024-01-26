# Malef
**Malef** is toolkit for writing terminal-based applications in Ada. It is
completely written in Ada and it has no dependencies.

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![License: FDL 1.3](https://img.shields.io/badge/License-FDL%20v1.3-blue.svg)](http://www.gnu.org/licenses/fdl-1.3)

## Description
**Malef** is an open source and free library written in Ada to create Terminal
User Interfaces. It divided in two parts: the _Drawing Toolkit_ and the
_Widget Toolkit_. The _Drawing Toolkit_ provides the lowlevel stuff with
drawing and composing primitives. And the _Widget Toolkit_ aims to simplify the
creation of Applications similarly as you would do by using a GUI toolkit such
as Gtk or Qt.

### The _Drawing Toolkit_
The _Drawing Toolkit_ provides a lowlevel API for drawing Surfaces,
composing them in _Boxes_ and showing them on the _Window_.

#### Surfaces
A `Surface` is just a 2-dimensional Matrix of Glyphs (`Wide_Wide_Character`s),
Colours and Styles. Drawing on them is very simple, you can take a look at
the `malef-surfaces.ads` package to see all the available functions to draw:

```ada
with Malef.Surfaces;

procedure Surface_Example is
   My_Surface : Malef.Surfaces.Surface (3, 12);   -- Specify the number of
                                                  -- Rows and Columns.
begin

   My_Surface (1, 1) := 'H';
   My_Surface.Put (1, 2, "ello, World");
   My_Surface.Fill_Background ((80, 80, 120, 255));
   My_Surface.Fill_Foreground ((1, 1), (1, 12), (200, 200, 200, 255));
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
