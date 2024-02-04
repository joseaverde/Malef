-------------------------------------------------------------------------------
--                                                                           --
--                             M A L E F . A D S                             --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   S P E C                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2024 José Antonio Verde Jiménez  All Rights Reserved  --
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

with Ada.Strings.Text_Buffers;

package Malef with
   Pure => True
   -- Global      => in out synchronized,
   -- Nonblocking => True
is

   -- Malef is a library for building cross-compatible Terminal User Interfaces
   -- (TUI) in Ada. In a simple, fast and intuitive manner. The library is
   -- separated in two public toolkits: the Drawing Toolkit and the Widget
   -- Toolkit.
   --
   -- The Drawing Toolkit (packages in src/drawing) contains the drawing
   -- primitives and the lowlevel stuff. The developer is given the Surface
   -- type, which is matrix of Glyphs, with Colours and Styles where one can
   -- draw anything. The you can compose Surfaces or (even other Groups) inside
   -- Groups, give the Opacity, the Layer_Mode and the Group type composes it
   -- for you. Finally, the Malef.Window protected object is in charged of
   -- receiving Events and notifying other objects and drawing on the Screen.
   -- If you want to create your own Widgets, you will need to use them.
   --
   -- The Widget Toolkit (packages in src/widgets) contains different types
   -- that implement different Widgets. You have Dialogs (akin to a pop-up
   -- window you can move on the screen) and Widgets (objects that are drawn
   -- on the Dialogs and which have some kind of functionality. The Widgets and
   -- Dialogs are stored in a tree-like structure, and events are passed down
   -- from the root widget (the Application) to the leaf widgets. You can
   -- specify your own callbacks to widgets, that will notify you when an
   -- event occurs.
   --
   -- Other packages under src/, may be used. But not those in the src/private
   -- or src/subsystems directories, those are used internally and are
   -- constantly changing. The only packages that use them are from the Drawing
   -- Toolkit.

   --<<--------->>--
   -->> Colours <<--
   --<<--------->>--

   -- Malef uses RGBA colours internally (Red, Green, Blue and Alpha). Alpha
   -- is the opacity: 255 is a completely opaque colour, and 0 is a completely
   -- transparent colour. Thanks to Ada 2022 Aspects you can declare colours
   -- using HTML tags:
   --
   --    Red : constant RGBA_Type := "#F00";
   --    -- Red => 255, Green => 0, Blue => 0, Alpha => 255
   --    -- It is equivalent to: "#FF0000", "#F00F" and "#FF0000FF"
   --
   --    Transparent : constant RGBA_Type := "#0000"
   --    -- Red => 0, Green => 0, Blue => 0, Alpha => 0
   --
   -- The colour tags start with a Hash character ('#') followed by hexadecimal
   -- values. If there are 3 or 4 characters, then they are duplicated. For
   -- instance: "#ABC" = "#AABBCC" and "#0A8F" = "#00AA88FF".
   --
   -- You can use the 'Value and 'Image attributes on RGBA_Type's.
   --
   -- Malef also Palettes, Palettes are described in the Malef.Palettes package
   -- with primitives for using them. Palettes are arrays 16-colour arrays that
   -- can be associated to Groups or Surfaces.

   type Component_Name is (Red, Green, Blue, Alpha);
   -- Identifies the names of the different RGBA_Colour.
   --
   -- @enum Red
   -- R is for Red, it is the amount of Red the colour has.
   --
   -- @enum Green
   -- G is for Green, it is the amount of Green the colour has.
   --
   -- @enum Blue
   -- B is for Blue, it is the amount of Blue the colour has.
   --
   -- @enum Alpha
   -- A is for Alpha, it is the colour opacity. A completely opaque colour has
   -- an Alpha value of 255 (16#FF#). A completely transparent colour has an
   -- Alpha value of 0 (16#00#).

   type Component_Type is mod 256 with
      Size      => 8,
      Put_Image => Put_Image;

   type RGBA_Type is array (Component_Name'Range) of Component_Type with
      Size           => 32,
      Put_Image      => Put_Image,
      String_Literal => Value;
   -- This type represent an RGBA colour, it is what Malef using internally for
   -- representing colours. You can declare new colours using arrays:
   --
   --    [34, 68, 102, 255]
   --
   -- or maybe
   --
   --    (34, 68, 102, 255)
   --
   -- Or using string literals
   --
   --    "#224466FF"
   --    "#224466"
   --    "#246F"
   --    "#246"
   --
   -- All of them are equivalent, but using arrays is faster.

   type Palette_Index is range 0 .. 15 with
      Size => 8;

   subtype Valid_Hexadecimal_Digit is Wide_Wide_Character with
      Static_Predicate => Valid_Hexadecimal_Digit in '0' .. '9'
                                                   | 'a' .. 'f'
                                                   | 'A' .. 'F';

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Component_Type);

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     RGBA_Type);

   function Value (
      Item : in Wide_Wide_String)
      return RGBA_Type with
      Pre      => (Item'Length in 4 | 5 | 7 | 9
         and then Item (Item'First) = '#'
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Item (I) in Valid_Hexadecimal_Digit))
         or else raise Constraint_Error;
   -- Given a Wide_Wide_String it tries to convert it to the RGBA_Type. It is
   -- used to implement the String_Literal aspect of the RGBA_Type.
   --
   -- @param Item
   -- A Wide_Wide_String with the representation of the colour. It should be
   -- valid (starts with '#' and it has 3, 4, 6 or 8 hexadecimal digits).

   --<<-------->>--
   -->> Styles <<--
   --<<-------->>--

   -- The Style represents the way text is printed on the screen. Malef
   -- supports several common styles. In case some style is not available on
   -- the backend, it will be emulated (even if it costs CPU power).
   --
   -- The Malef.Labels.Label_Widget type allows certain types of markup
   -- languages for styles (Markdown...), but you can use the Style directly.

   type Style_Name is (Bold,        Faint,       Italic,        Underline,
                       Slow_Blink,  Rapid_Blink, Reverse_Video, Conceal,
                       Crossed_Out);
   -- The names that identify each style.
   --
   -- @enum Bold
   -- Bold characters have a thicker stroke. It is usually used to emphasise
   -- text. If it is not available: Underline or Italic or Reverse will be used
   -- instead (in that order). This Style cannot be emulated.
   --
   -- @enum Faint
   -- Faint characters have a dimmer colour or/and a slimmer stroke. It is
   -- usually used to represent text that doesn't matter. If it is not
   -- available, then a dimmer colour will be used (there is no other way to
   -- emulate this)
   --
   -- @enum Italic
   -- Italic characters are usually inclined. They are used to emphasise, for
   -- quoting or for loan words. If it is not available, Underline or Bold or
   -- Reverse will be used instead. This Style cannot be emulated.
   --
   -- @enum Underline
   -- Underlined characters have a line under them. It is usually used to
   -- emphasise text. If not available, Bold or Italic or Reverse will be used
   -- instead. This Style cannot be emulated.
   --
   -- @enum Slow_Blink
   -- It shows and hides the character less than 150 times per minute (2.5 per
   -- second). If it is not available Rapid_Blink will be used if available or
   -- a task will be created that will blink the character manually.
   --
   -- @enum Rapid_Blink
   -- It shows and hides the character more than 150 per minute (2.5 per
   -- second). If it is not available Slow_Blink will be used instead.
   --
   -- @enum Reverse_Video
   -- Swaps the Foreground with the Background. If not available it is simple
   -- to emulate.
   --
   -- @enum Conceal
   -- Hides the character, it is not widely available but it can be emulated.
   --
   -- @enum Crossed_Out
   -- The characters are visible but marked as if for deletion. If not
   -- available a Red background with Yellow foreground will be used instead to
   -- mark it as a warning.

   type Style_Type is array (Style_Name) of Boolean with Pack;

   No_Style : constant Style_Type := (others => False);

   --<<--------->>--
   -->> Cursors <<--
   --<<--------->>--

   -- Positions in Malef are given: first the row (Y-coordinate), next the
   -- column (X-coordinate). Most TUI libraries use this convention. It is more
   -- natural to think of character whithin a line; instead of a character
   -- whithin a column. It is also the same way matrices are indexed in linear
   -- algebra.
   --
   -- For compatibility the rows and columns will use the range of a 16-bit
   -- integer. There is no way a screen will have 65_536 characters in the near
   -- future if there aren't even 65_536x65_536 pixel monitors yet. So don't
   -- worry about it. In any case it is better to use attributes instead of
   -- magic numbers: Row_Type'First instead of -32_768.

   type Row_Type is range -32_768 .. 32_767;
   subtype Row_Count is Row_Type range 0 .. Row_Type'Last;
   subtype Positive_Row_Count is Row_Type range 0 .. Row_Type'Last;

   type Col_Type is range -32_768 .. 32_767;
   subtype Col_Count is Col_Type range 0 .. Col_Type'Last;
   subtype Positive_Col_Count is Col_Type range 0 .. Col_Type'Last;

   type Cursor_Type is
      record
         Row : Row_Type;
         Col : Col_Type;
      end record;
   -- A cursor represents a position on the screen.

   --<<------------>>--
   -->> Characters <<--
   --<<------------>>--

   subtype Glyph is Wide_Wide_Character;
   -- Glyph is just an alias to Wide_Wide_Character. In the past it was just a
   -- Character and then a Wide_Character. The type alias won't change within
   -- the same major version once the first major version is relased. There are
   -- no plans on changing this in the future.

   subtype Glyph_String is Wide_Wide_String;
   -- A Glyph_String is just a String of Glyphs. It is an alias to a
   -- Wide_Wide_String for convenience.

   type Glyph_Block is array (Positive range <>, Positive range <>) of Glyph;
   -- A Glyph_Block is a Matrix of Glyphs.

   Nul : constant Glyph := Glyph'First;
   -- This character is completely transparent: if there is a character under
   -- it, the character below will be shown instead. It is used as the default
   -- character in Surfaces.

   --<<------------>>--
   -->> Exceptions <<--
   --<<------------>>--

   Initialization_Error : exception;
   -- This exception is raised when the library couldn't be initialized (and
   -- maybe finalized). This error is uncommon to see, everything should work
   -- pretty much everywhere. Treat it like a Program_Error.

end Malef;
