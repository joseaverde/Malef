-------------------------------------------------------------------------------
--                                                                           --
--                             M A L E F . A D S                             --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2021 José Antonio Verde Jiménez  All Rights Reserved  --
-------------------------------------------------------------------------------
-- This file is part of Malef.                                               --
--                                                                           --
-- This program is free software:  you  can redistribute it and/or modify it --
-- under  the terms  of the  GNU  General License  as published by the  Free --
-- Software  Foundation,  either  version 3  of  the  License,  or  (at your --
-- opinion) any later version.                                               --
--                                                                           --
-- This  program  is distributed  in the  hope that  it will be  useful, but --
-- WITHOUT   ANY   WARRANTY;   without   even  the   implied   warranty   of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.                                          --
--                                                                           --
-- You should have received  a copy of the  GNU General Public License along --
-- with this program. If not, see <https://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

private with System.Atomic_Counters;
limited with Malef.Surfaces;

--
-- @summary
-- This is the parent package of the Malef library, it declares most of the
-- types used thoughout the whole library and the initialiation and
-- finalization procedures.
--
-- @description
-- This package declares the numeric and string types used in this package.
-- Also other `record' types like Cursor_Type and the private
-- Shared_Surface_Type which will be later derived into more types. For example
-- the Surface_Type will use the Shared_Surface_Type as a component, but will
-- be declared in a different child package Malef.Surfaces.
-- It also declares the colour and the style types.
--
package Malef is


   ---============-------------============---
   --============-- T Y P E S --============--
   ---============-------------============---

   --====-------------====--
   --====-- COLOURS --====--
   --====-------------====--
   --
   -- Here we will be declaring the colour types.
   -- In every terminal/console there must be at least two colours (even if
   -- they aren't techinally called collors) which are black and white, so the
   -- text can be read.
   --
   -- Terminals which support colours at leas support eight colours (and their
   -- bright variants) these are the 3/4-bits colours.
   --
   -- Other terminals support 8-bits colours, whose first 16 colours are
   -- reserved for the 3/4-bits colours and the last values are for shades of
   -- grey.
   --
   -- Finally we have the common 3-bytes (24 bits) RGB colours.
   --
   -- However this library supports for 32-bits-colour applications, the RGBA
   -- is used (A stands for Alpha which is opacity). And those colours are
   -- converted directly into the colours supported by the system. That way we
   -- don't have to declare different types for different systems, only a
   -- global one is needed, and is the one to be converted). The transparency
   -- only works when showing layers, because the bottom layer will be
   -- considered and made completely black so there are no inconsistencies when
   -- working with a semi-transparent terminal/console (which are so popular
   -- right now).
   --
   -- (In this library's comments I'm using British English, even though the
   --  naming conventions in this library are in American English. So keep that
   --  in mind).
   --

   --
   -- This is type for the colour components. They are 8 bits each one of them.
   --
   type Color_Component_Type is mod 256;
   for Color_Component_Type'Size use 8;

   --
   -- These are the names for each of the components of the Color_Type, so no
   -- numeric value needs to be used. Color(0) is ugly and hard to know what's
   -- happening, but Color(R) doesn't.
   --
   -- @value R
   -- The amount of Red the colour has.
   --
   -- @value G
   -- The amount of Green the colour has.
   --
   -- @value B
   -- The amount of Blue the colour has.
   --
   -- @value A
   -- The opacity of the colour: 255 is completely opaque and 0 is completely
   -- transparent.
   --
   type Color_Component_Kind is (R, G, B, A);
   for Color_Component_Kind'Size use 2;

   --
   -- This is the colour type used in this library, read the comments above for
   -- more information. It's a 4 byte array of Color_Component_Types, which are
   -- 1 byte unsigned values ranging from 0 to 255.
   --
   type Color_Type is array (Color_Component_Kind'Range)
                      of     Color_Component_Type;
   for Color_Type'Size use 32;



   --====------------====--
   --====-- STYLES --====--
   --====------------====--
   --
   -- Here we will declare the style types.
   -- There are certain styles which aren't supported by default by certain
   -- consoles or terminals, so their effect we will try to recreate the same
   -- effect in library. For example: Doubly_Underline will be put as a single
   -- Underline. Others like the blinking effect will blink each time the
   -- terminal is update to replicate the effect.
   --

   --
   -- These are the supported styles, keep in mind not all of them are
   -- avaiable by default in every available terminal out there, so the effects
   -- will be (tried to be) recreated.
   -- Most of the information given here has been taken from this Wikipedia
   -- article:
   --    https://en.wikipedia.org/wiki/ANSI_escape_code
   --
   -- @value Bold
   -- Bold (Replicable if 8-bits or 24-bits colours are available).
   --
   -- @value Faint
   -- Also known as Dim (with a saturated colour). (Replicable if 8-bits or
   -- 24-bits colours are available).
   --
   -- @value Italic
   -- Not widely supported. Sometimes treated as inverse or blink. (Replicable
   -- with Inverse or Blink).
   --
   -- @value Underline
   -- Style extensions exist for Kitty, VTE, mintty and iTerm2. (Non-Replicable
   -- maybe Inverse).
   --
   -- @value Slow_Blink
   -- Less than 150 per minute. (Replicable, but may be avoided).
   --
   -- @value Rapid_Blink
   -- 150+ per minute; not widely supported. (Replicable, maybe with a
   -- Slow_Blink.
   --
   -- @value Reverse_Video
   -- Swap foreground and background colours, also known as invert;
   -- inconsistent emultaion. (Replicable, and will be replicated as much as
   -- possible because it's faster to change colours that must be put in either
   -- case than putting another string).
   --
   -- @value Conceal
   -- Also known as Hide, not widely supported. (Replicable).
   --
   -- @value Crossed_Out
   -- Also known as String, characters legible but marked as if for deletion.
   -- (Non-Replicable).
   --
   -- @value Doubly_Underline
   -- A double underline (Replicable with a single Underline).
   --
   type Style_Type is (Bold,        Faint,           Italic,        Underline,
                       Slow_Blink,  Rapid_Blink,     Reverse_Video, Conceal,
                       Crossed_Out, Doubly_Underline);
   for Style_Type'Size use 4;

   --
   -- This is just an array of styles telling whether they are put or not.
   --
   type Style_Array is array (Style_Type'Range) of Boolean;
   pragma Pack (Style_Array);



   --====-------------====--
   --====-- FORMATS --====--
   --====-------------====--
   --
   -- In this part we declare the formats, which are a specific format that can
   -- be put all together without needing to first put the colours and then the
   -- styles.
   --

   --
   -- This type contains the information needed to put a specific format into
   -- the screen.
   --
   -- @field Foreground_Color
   -- The colour of the letters.
   --
   -- @field Background_Color
   -- The colour of the letters' background.
   --
   -- @field Styles
   -- The styles it has.
   --
   type Format_Type is
      record
         Foreground_Color : Color_Type;
         Background_Color : Color_Type;
         Styles           : Style_Array;
      end record;
   pragma Pack (Format_Type);

   -- This is the default format.
   Default_Format :  constant Format_Type
                  := Format_Type'(Foreground_Color => (255, 255, 255, 255),
                                  Background_Color => (  0,   0,   0, 255),
                                  Styles           => (others => False));



   --====-------------====--
   --====-- CURSORS --====--
   --====-------------====--
   --
   -- Here we declare the column and row types and also the cursor type which
   -- is just a record type containing the row and the column the cursor is
   -- found at. It is tagged, however, so a Moveable_Cursor_Type can inherit
   -- from it.
   --
   -- We have to differenciate between coordinates and counting types:
   --    Row_Coord : Is to specify where will the object printed.
   --    Row_Type  : Is to specify the number of rows of the object or to index
   --                a position is an object.
   --

   --
   -- This type is used to count rows.
   --
   type Row_Type is new Positive range 1 .. 1024
      with Size => 16;

   --
   -- This type is used to count columns. I prefer to call it Col instead of
   -- the full name Column so it can have the same number of letters as Row.
   --
   type Col_Type is new Positive range 1 .. 1024
      with Size => 16;


   --
   -- This type is used to represent the row where something is placed. The
   -- row number zero is the one just above the top side. Coordinates start
   -- with zero for compatibility with the ANSI terminals.
   --
   type Row_Coord is range -2**15 .. 2**15-1
      with Size => 16;

   --
   -- This type is used to represent the coloumn where something is placed in a
   -- terminal. The columns begin with 1, any value lower is always hidden
   -- because it's always at the left side of the terminal.
   --
   type Col_Coord is range -2**15 .. 2**15-1
      with Size => 16;

   --
   -- This is the cursors type. It can be used to tell the position in the
   -- visible part of the terminal or inside a surface itself.
   --
   -- @field Row
   -- The row the cursor is at.
   --
   -- @field Col
   -- The column the cursor is at.
   --
   type Cursor_Type is tagged
      record
         Row : Row_Type;
         Col : Col_Type;
      end record;

   --
   -- The Coord type is similar to the Cursor type, however the cursor type can
   -- only be placed inside an object like a surface, but this can be placed
   -- anywhere in the terminal. For example to tell the position of a surface.
   --
   -- @field Row
   -- The row where it is.
   --
   -- @field Col
   -- The column where it is.
   --
   type Coord_Type is tagged
      record
         Row : Row_Coord;
         Col : Col_Coord;
      end record;


   --====----------------------------====--
   --====-- CHARACTERS AND STRINGS --====--
   --====----------------------------====--
   --
   -- Here we declare the string and the character types used in this package
   -- which are just Wide_Wide_Characters in the form of a 4 byte array used
   -- to codify 8-UTF strings in every system.
   --

   --
   -- The components of a character type.
   --
   type Char_Component_Type is mod 256;
   for Char_Component_Type'Size use 8;

   --
   -- This type is this library's character type. It's used to store unicode.
   -- TODO: Conversors.
   --
   type Char_Type is array (Positive range 1 .. 2) of Char_Component_Type
      with Pack;

   --
   -- This is the string type in the Malef package which is an array of
   -- Char_Type.
   --
   type Str_Type is array (Positive range <>) of Char_Type;


   -- Don't look at this, you can't do anything with it.
   type Shared_Surface_Access is private;


   --====----------------------------====--
   --====-- INITIALIZATION OPTIONS --====--
   --====----------------------------====--
   --
   -- These types are used to specify which way Malef should be initialised.
   --

   --
   -- These are the different subsystems there are.
   --
   -- @value Choose
   -- When the Choose flag is set, then the best one will be chosen. By default
   -- the best option is always Ansi because it's this library strong point.
   --
   -- @value ANSI
   -- This flag should be closs-platform (except for older Windows versions).
   -- It uses ANSI Escape Sequences to control the terminal. Keep in mind it
   -- needs Standard_Output to be working.
   --
   -- @value CMD
   -- This flag is used for older windows terminals. It may be a little slow
   -- because Windows API functions must be called to control the terminal, and
   -- thus increasing the number of calls to print onto the screen. And thus
   -- decreasing performance.
   --
   -- @value Ncurses
   -- This flag is currently experimental, it's a kind of wrap up for Ncurses
   -- functions for Bench Marking and also to allow other terminals that can
   -- use Ncurses but don't allow any other subsystem to be able to run Malef
   -- programs.
   --
   type Subsystem_Kind is (Choose, ANSI, CMD, Ncurses);

   ---============-----------------------============---
   --============-- O P E R A T I O N S --============--
   ---============-----------------------============---

   --====-------------------------------------====--
   --====-- INITIALIZATION AND FINALIZATION --====--
   --====-------------------------------------====--
   --
   -- This functions are a MUST when using this library for everything to work.
   -- You can use surfaces and work with them before initializing the library,
   -- but some functions require the library to be initialized.
   --

   --
   -- This function prepares the terminal with operating system specific
   -- functions.
   --
   -- @exception Malef.Exceptions.Initialization_Error
   -- This exception is raised if it has already been initialized.
   --
   procedure Initialize;

   --
   -- This procedure finalises the Malef package.
   --
   -- @exception Malef.Exceptions.Initialization_Error
   -- This exceptions is raised if it hasn't been initialized yet.
   --
   procedure Finalize;

   --
   -- This function returns whether the library has been initialized.
   --
   function Is_Initialized return Boolean;



   --====-----------------------------------====--
   --====-- TERMINAL CONTROL AND HANDLING --====--
   --====-----------------------------------====--
   --
   -- This part contains functions and procedures to interact with the terminal
   -- or the console directly.
   --

   --
   -- This returns the height of the terminal.
   --
   -- @return
   -- Returns the height of the terminal.
   --
   -- @exception Malef.Exceptions.Initialization_Error
   -- This exception is raised if Malef hasn't been properly initialized.
   --
   function Get_Height return Row_Type;

   --
   -- This returns the width of the terminal.
   --
   -- @return
   -- Returns the width of the terminal.
   --
   -- @exception Malef.Exceptions.Initialization_Error
   -- This exception is raised if Malef hasn't been properly initialized.
   --
   function Get_Width return Col_Type;

   --
   -- This procedure creates a new page, that is, adds enough lines to the
   -- terminal without removing what was before.
   --
   -- @exception Malef.Exceptions.Initialization_Error
   -- This exception is raised if it hasn't been properly initialized.
   --
   procedure New_Page;

   --
   -- This procedure changes the title of the terminal.
   --
   -- @param Name
   -- The new name.
   --
   -- @exception Malef.Exceptions.Initialization_Error
   -- This exception is raised if Malef hasn't been properly initialized.
   --
   procedure Set_Title (Name : String);

   --
   -- This function updates the terminal size.
   --
   -- @return
   -- Returns whether the terminal size has been updated.
   --
   -- @exception Malef.Exceptions.Initialization_Error
   -- This exception is raised if it hasn't been properly initialized.
   --
   function Update_Terminal_Size return Boolean;


--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
private --*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--
--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-

   --====--------------====--
   --====-- SURFACES --====--
   --====--------------====--
   --
   -- Surfaces is what this package is all about, the ability to use a
   -- sprite-like data type to handle the terminal io as fast as possible.
   --

   --
   -- These are the elements of the Surface's grid (the matrix).
   --
   -- @field Format
   -- The format of each of the characters of the matrix.
   --
   -- @field Char
   -- The character it is.
   --
   type Element_Type is
      record
         Format : Format_Type;
         Char   : Char_Type;
      end record;

   --
   -- This is the Matrix_Type, the matrix where every element of the Surface is
   -- stored.
   --
   type Matrix_Type is array (Row_Type range <>, Col_Type range <>)
                       of     Element_Type;

   --
   -- This is the real Surface_Type although it needs a kind of wrapper which
   -- is called Surface_Type in order to have Controlled types and Garbage
   -- Collection. Surface_Type is declared at Malef.Surfaces and has a similar
   -- specification to the one Unbounded_Strings in Ada.Strings.Unbounded have.
   --
   -- @field Height
   -- The height of the surface.
   --
   -- @field Width
   -- The width of the surface.
   --
   -- @field Grid
   -- The insides of the Surface, the information that is shown.
   --
   -- @field Position
   -- The position of the Surface in the terminal.
   --
   -- @field Cursor_Format
   -- The default format of the cursor, i.e. the default writing colour that is
   -- used when writing inside the surface.
   --
   -- @field Cursor_Position
   -- The position of the Cursor in the Surface.
   --
   -- @field Counter
   -- This counter counts the number of objects that contain a shared surface,
   -- when this number reaches zero, the surface is deallocated. Except for the
   -- Shared_Null_Surface.
   --
   -- @field Writable
   -- If this variable is false then it can't be modified by the user by normal
   -- methods, for example calling a function. The removes the need to have a
   -- null surface and check whether it is one of them.
   --
   type Shared_Surface_Type (Height : Row_Type;
                             Width  : Col_Type) is limited
      record
         Grid            : Matrix_Type (1 .. Height, 1 .. Width);
         Position        : Cursor_Type := Cursor_Type'(Row => 1,
                                                       Col => 1);

         Cursor_Format   : Format_Type := Default_Format;
         Cursor_Position : Cursor_Type := Cursor_Type'(Row => 1,
                                                       Col => 1);

         Counter         : System.Atomic_Counters.Atomic_Counter;

         Writable        : Boolean ;
      end record;

   --
   -- This type is an access type for the Shared_Surface_Type, it's public
   -- but users can't do anything with it. It's only used for the library's
   -- internals.
   --
   type Shared_Surface_Access is access all Shared_Surface_Type;

   --
   -- This surface is shown for every Surface_Type that hasn't been yet
   -- initialized.
   --
   Shared_Null_Surface : aliased Shared_Surface_Type (Height => 1,
                                                      Width  => 1) :=
      Shared_Surface_Type'(
         Height   => 1,
         Width    => 1,
         Grid     => (others => (others => Element_Type'(
            Format => Format_Type'(Foreground_Color => (255, 255,   0, 255),
                                   Background_Color => (255,   0,   0, 255),
                                   Styles           => (others => False)),
            Char   => (0, Character'Pos('?')) ) )),
         Writable => False,
         others => <>
      );

   --
   -- This function returns the shared surface contained by a Surface_Type so
   -- other child packages can access and modify it.
   --
   -- @param Object
   -- The object to retrieve the shared surface from.
   --
   -- @return
   -- The shared surface.
   --
   function Get_Shared_Surface (Object : Malef.Surfaces.Surface_Type)
                                return Shared_Surface_Access;
   pragma Inline (Get_Shared_Surface);


   --====---------------====--
   --====-- VARIABLES --====--
   --====---------------====--

   -- Whether the library has been initialized.
   Has_Been_Initialized : Boolean := False;

   -- The size of the terminal.
   Height : Row_Type := 80;
   Width  : Col_Type := 24;


end Malef;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
