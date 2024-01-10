-------------------------------------------------------------------------------
--                                                                           --
--                    M A L E F - S U R F A C E S . A D S                    --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
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

private with Malef.Implementation;
with Ada.Strings.Text_Buffers;
with Malef.Palettes;

package Malef.Surfaces with Pure is

   -- This package defines the Surface type and its primitive operations.
   --
   -- A Surface is a 2-dimensional array of characters with attributes. Each
   -- character has indexed or not background, foreground colours and style.
   -- For instance, imagine a Message Dialog that contains "Hello, World!",
   -- which is shown as follows:
   --
   --    +---------------+
   --    | Hello, World! |#
   --    +---------------+#
   --     #################
   --
   -- The Dialog (without the shadow) is represented as follows:
   --
   --    ["+" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "+"]
   --    ["|" " " "H" "e" "l" "l" "o" "," " " "W" "o" "r" "l" "d" "!" " " "|"]
   --    ["+" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "+"]
   --
   -- And each "Cell" contains a Background and Foreground colour. The
   -- Foreground colour, is the colour of the Glyph itself. And the background
   -- is the colour of -- as its name implies -- the background below the
   -- glyph.
   --
   -- This package provides functions to modify Surfaces. Internally, only the
   -- modified sections are updated, so you don't have to worry the system has
   -- to redraw everything if you change a single cell in the Surface.
   --
   -- Thanks to Constant_Indexing and Variable_Indexing aspects you can work
   -- with surfaces like:
   --
   --    declare
   --       My_Surface : Surface (10, 10);
   --       My_Style   : constant Style_Type := (Bold   => True,
   --                                            others => False);
   --       Yellow     : constant RGBA_Type := (255, 255, 0, 255);
   --       Message    : constant String := "Hello, World!";
   --    begin
   --       My_Surface.Background (2, 3) := Yellow;
   --       My_Surface (2, 3) := My_Style;
   --       My_Surface (2, 3) := 'H';
   --       for I in Message'Range loop
   --          My_Surface (2, 3 + I - Message'First) := Message (I);
   --       end loop;
   --    end;
   --
   -- In the future more functions will be added for convenience.

   type Surface (
      Rows : Positive_Row_Count;
      Cols : Positive_Col_Count) is
      tagged private with
      Variable_Indexing         => Reference,
      Constant_Indexing         => Constant_Reference,
      Put_Image                 => Put_Image,
      Default_Initial_Condition =>
         Modified (Surface) and then
         (for all Row in 1 .. Rows =>
            (for all Col in 1 .. Cols =>
               (not Is_Indexed (Surface, Row, Col))                  and then
               (Get (Surface, Row, Col) = ' ')                       and then
               (Get (Surface, Row, Col) = No_Style)                  and then
               (Get_Background (Surface, Row, Col) = (0, 0, 0, 0))   and then
               (Get_Foreground (Surface, Row, Col) = (0, 0, 0, 0))));
   -- A Surface is a 2-dimensional array of Glyphs with Attributes (colours...)
   -- which is associated to a Palette. Surface are indexed from (1, 1) to
   -- (Rows, Cols), any value outside that range raises an exception.
   --
   -- A Surface by default doesn't have any style it is full of spaces (' ')
   -- and its background is Black, with White foreground.

   --<<-------------->>--
   -->> Helper Types <<--
   --<<-------------->>--

   type Palette_Reference_Type (
      Element : not null access Palettes.Palette_Type) is
      limited null record with Implicit_Dereference => Element;

   type RGBA_Reference_Type (
      Element : not null access RGBA_Type) is
      limited null record with Implicit_Dereference => Element;

   type Palette_Index_Reference_Type (
      Element : not null access Palette_Index) is
      limited null record with Implicit_Dereference => Element;

   type Glyph_Reference_Type (
      Element : not null access Glyph) is
      limited null record with Implicit_Dereference => Element;

   type Palette_Constant_Reference_Type (
      Element : not null access constant Palettes.Palette_Type) is
      limited null record with Implicit_Dereference => Element;

   type Style_Reference_Type (
      Element : not null access Style_Type) is
      limited null record with Implicit_Dereference => Element;

   type Palette_Index_Constant_Reference_Type (
      Element : not null access constant Palette_Index) is
      limited null record with Implicit_Dereference => Element;

   type Glyph_Constant_Reference_Type (
      Element : not null access constant Glyph) is
      limited null record with Implicit_Dereference => Element;

   type Style_Constant_Reference_Type (
      Element : not null access constant Style_Type) is
      limited null record with Implicit_Dereference => Element;

   --<<------------------>>--
   -->> Helper Functions <<--
   --<<------------------>>--

   function In_Range (
      Object : in Surface;
      Row    : in Row_Type)
      return Boolean is (
      Row in 1 .. Object.Rows);

   function In_Range (
      Object : in Surface;
      Col    : in Col_Type)
      return Boolean is (
      Col in 1 .. Object.Cols);

   function In_Range (
      Object : in Surface;
      Row    : in Row_Type;
      Col    : in Col_Type)
      return Boolean is (
      In_Range (Object, Row) and then In_Range (Object, Col));

   function In_Range (
      Object : in Surface;
      Cursor : in Cursor_Type)
      return Boolean is (
      In_Range (Object, Cursor.Row) and then In_Range (Object, Cursor.Col));

   function In_Range (
      Object : in Surface;
      From   : in Cursor_Type;
      To     : in Cursor_Type)
      return Boolean is (
      In_Range (Object, From) and then In_Range (Object, To)
      and then To.Row >= From.Row and then To.Col >= From.Col);

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Surface) with
      Global => null;

   function Is_Indexed (
      Object : in Surface;
      Row    : in Row_Type;
      Col    : in Col_Type)
      return Boolean with
      Pre    => In_Range (Object, Row, Col) or else raise Constraint_Error,
      Global => null;

   --<<-------------->>--
   -->> Surface Info <<--
   --<<-------------->>--

   function Modified (
      Object : in Surface)
      return Boolean with
      Global => null;

   procedure Set_Up_to_Date (
      Object : in out Surface) with
      Post   => not Modified (Object),
      Global => null;

   --<<-------------------->>--
   -->> On Cell Operations <<--
   --<<-------------------->>--

   -->> Glyphs <<--

   function Reference (
      Object : aliased in out Surface;
      Row    :         in     Positive_Row_Count;
      Col    :         in     Positive_Col_Count)
      return Glyph_Reference_Type with
      Pre    => In_Range (Object, Row, Col) or else raise Constraint_Error,
      Post   => Modified (Object),
      Global => null;

   function Constant_Reference (
      Object : aliased in Surface;
      Row    :         in Positive_Row_Count;
      Col    :         in Positive_Col_Count)
      return Glyph_Constant_Reference_Type with
      Pre    => In_Range (Object, Row, Col) or else raise Constraint_Error,
      Global => null;

   function Get (
      Object : in Surface;
      Row    : in Positive_Row_Count;
      Col    : in Positive_Col_Count)
      return Glyph with
      Pre    => In_Range (Object, Row, Col) or else raise Constraint_Error,
      Global => null;

   procedure Set (
      Object : in out Surface;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Item   : in     Glyph) with
      Pre    => In_Range (Object, Row, Col) or else raise Constraint_Error,
      Post   => Get (Object, Row, Col) = Item and then Modified (Object),
      Global => null;

   -->> Styles <<--

   function Reference (
      Object : aliased in out Surface;
      Row    :         in     Positive_Row_Count;
      Col    :         in     Positive_Col_Count)
      return Style_Reference_Type with
      Pre    => In_Range (Object, Row, Col) or else raise Constraint_Error,
      Post   => Modified (Object),
      Global => null;

   function Constant_Reference (
      Object : aliased in Surface;
      Row    :         in Positive_Row_Count;
      Col    :         in Positive_Col_Count)
      return Style_Constant_Reference_Type with
      Pre    => In_Range (Object, Row, Col) or else raise Constraint_Error,
      Global => null;

   function Get (
      Object : in Surface;
      Row    : in Positive_Row_Count;
      Col    : in Positive_Col_Count)
      return Style_Type with
      Pre    => In_Range (Object, Row, Col) or else raise Constraint_Error,
      Global => null;

   procedure Set (
      Object : in out Surface;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Item   : in     Style_Type) with
      Pre    => In_Range (Object, Row, Col) or else raise Constraint_Error,
      Post   => Get (Object, Row, Col) = Item and then Modified (Object),
      Global => null;

   -->> Colours <<--

   function Background (
      Object : aliased in out Surface;
      Row    :         in     Positive_Row_Count;
      Col    :         in     Positive_Col_Count)
      return RGBA_Reference_Type with
      Pre      => (In_Range (Object, Row, Col) and then
                   not Is_Indexed (Object, Row, Col))
         or else  (raise Constraint_Error),
      Post     => Modified (Object),
      Global   => null;

   function Foreground (
      Object : aliased in out Surface;
      Row    :         in     Positive_Row_Count;
      Col    :         in     Positive_Col_Count)
      return RGBA_Reference_Type with
      Pre      => (In_Range (Object, Row, Col) and then
                   not Is_Indexed (Object, Row, Col))
         or else  (raise Constraint_Error),
      Post     => Modified (Object),
      Global   => null;

   function Get_Background (
      Object : in Surface;
      Row    : in Positive_Row_Count;
      Col    : in Positive_Col_Count)
      return RGBA_Type with
      Pre      => (In_Range (Object, Row, Col) or else raise Constraint_Error),
      Global   => null;

   function Get_Foreground (
      Object : in Surface;
      Row    : in Positive_Row_Count;
      Col    : in Positive_Col_Count)
      return RGBA_Type with
      Pre      => (In_Range (Object, Row, Col) or else raise Constraint_Error),
      Global   => null;

   procedure Set (
      Object     : in out Surface;
      Row        : in     Positive_Row_Count;
      Col        : in     Positive_Col_Count;
      Background : in     RGBA_Type;
      Foreground : in     RGBA_Type) with
      Pre      => (In_Range (Object, Row, Col) or else raise Constraint_Error),
      Post     => not Is_Indexed (Object, Row, Col)
         and then Get_Background (Object, Row, Col) = Background
         and then Get_Foreground (Object, Row, Col) = Foreground
         and then Modified (Object),
      Global   => null;

   procedure Set_Background (
      Object : in out Surface;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Item   : in     RGBA_Type) with
      Pre      => (In_Range (Object, Row, Col) or else raise Constraint_Error),
      Post     => not Is_Indexed (Object, Row, Col)
         and then Get_Background (Object, Row, Col) = Item
         and then Modified (Object),
      Global   => null;

   procedure Set_Foreground (
      Object : in out Surface;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Item   : in     RGBA_Type) with
      Pre      => (In_Range (Object, Row, Col) or else raise Constraint_Error),
      Post     => not Is_Indexed (Object, Row, Col)
         and then Get_Foreground (Object, Row, Col) = Item
         and then Modified (Object),
      Global   => null;

   -->> Palettes <<--

   function Background_Id (
      Object : aliased in out Surface;
      Row    :         in     Positive_Row_Count;
      Col    :         in     Positive_Col_Count)
      return Palette_Index_Reference_Type with
      Pre      => (In_Range (Object, Row, Col) and then
                   Is_Indexed (Object, Row, Col))
         or else  (raise Constraint_Error),
      Post   => Modified (Object),
      Global => null;

   function Foreground_Id (
      Object : aliased in out Surface;
      Row    :         in     Positive_Row_Count;
      Col    :         in     Positive_Col_Count)
      return Palette_Index_Reference_Type with
      Pre      => (In_Range (Object, Row, Col) and then
                   Is_Indexed (Object, Row, Col))
         or else  (raise Constraint_Error),
      Post     => Modified (Object),
      Global   => null;

   function Get_Background (
      Object : in Surface;
      Row    : in Positive_Row_Count;
      Col    : in Positive_Col_Count)
      return Palette_Index with
      Pre      => (In_Range (Object, Row, Col) or else raise Constraint_Error),
      Global   => null;

   function Get_Foreground (
      Object : in Surface;
      Row    : in Positive_Row_Count;
      Col    : in Positive_Col_Count)
      return Palette_Index with
      Pre      => (In_Range (Object, Row, Col) or else raise Constraint_Error),
      Global   => null;

   procedure Set (
      Object     : in out Surface;
      Row        : in     Positive_Row_Count;
      Col        : in     Positive_Col_Count;
      Background : in     Palette_Index;
      Foreground : in     Palette_Index) with
      Pre      => (In_Range (Object, Row, Col) or else raise Constraint_Error),
      Post     => Is_Indexed (Object, Row, Col)
         and then Get_Background (Object, Row, Col) = Background
         and then Get_Foreground (Object, Row, Col) = Foreground
         and then Modified (Object),
      Global   => null;

   procedure Set_Background (
      Object : in out Surface;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Item   : in     Palette_Index) with
      Pre      => (In_Range (Object, Row, Col) or else raise Constraint_Error),
      Post     => not Is_Indexed (Object, Row, Col)
         and then Get_Background (Object, Row, Col) = Item
         and then Modified (Object),
      Global   => null;

   procedure Set_Foreground (
      Object : in out Surface;
      Row    : in     Positive_Row_Count;
      Col    : in     Positive_Col_Count;
      Item   : in     Palette_Index) with
      Pre      => (In_Range (Object, Row, Col) or else raise Constraint_Error),
      Post     => not Is_Indexed (Object, Row, Col)
         and then Get_Foreground (Object, Row, Col) = Item
         and then Modified (Object),
      Global   => null;

   -->> Omni <<--

   procedure Set (
      Object     : in out Surface;
      Row        : in     Positive_Row_Count;
      Col        : in     Positive_Col_Count;
      Background : in     RGBA_Type;
      Foreground : in     RGBA_Type;
      Character  : in     Glyph;
      Style      : in     Style_Type) with
      Pre      => (In_Range (Object, Row, Col) or else raise Constraint_Error),
      Post     => Is_Indexed (Object, Row, Col)
         and then Get_Background (Object, Row, Col) = Background
         and then Get_Foreground (Object, Row, Col) = Foreground
         and then Get (Object, Row, Col) = Character
         and then Get (Object, Row, Col) = Style
         and then Modified (Object),
      Global   => null;

   procedure Set (
      Object     : in out Surface;
      Row        : in     Positive_Row_Count;
      Col        : in     Positive_Col_Count;
      Background : in     Palette_Index;
      Foreground : in     Palette_Index;
      Character  : in     Glyph;
      Style      : in     Style_Type) with
      Pre      => (In_Range (Object, Row, Col) or else raise Constraint_Error),
      Post     => not Is_Indexed (Object, Row, Col)
         and then Get_Background (Object, Row, Col) = Background
         and then Get_Foreground (Object, Row, Col) = Foreground
         and then Get (Object, Row, Col) = Character
         and then Get (Object, Row, Col) = Style
         and then Modified (Object),
      Global   => null;

   --<<---------------------->>--
   -->> On Ranges Operations <<--
   --<<---------------------->>--

   -->> Glyphs <<--

   procedure Fill (
      Object : in out Surface;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type;
      Item   : in     Glyph) with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => (for all Row in From.Row .. To.Row =>
                   (for all Col in From.Col .. To.Col =>
                      (Get (Object, Row, Col) = Item)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object : in out Surface;
      Item   : in     Glyph) with
      Post   => (for all Row in 1 .. Object.Rows =>
                   (for all Col in 1 .. Object.Cols =>
                      (Get (Object, Row, Col) = Item)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object  : in out Surface;
      From    : in     Cursor_Type;
      To      : in     Cursor_Type;
      Process : not null access procedure (Item     : in out Glyph;
                                           Position : in     Cursor_Type)) with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => Modified (Object),
      Global => null;

   procedure Fill (
      Object  : in out Surface;
      Process : not null access procedure (Item     : in out Glyph;
                                           Position : in     Cursor_Type)) with
      Post   => Modified (Object),
      Global => null;

   -->> Styles <<--

   procedure Fill (
      Object : in out Surface;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type;
      Item   : in     Style_Type) with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => (for all Row in From.Row .. To.Row =>
                   (for all Col in From.Col .. To.Col =>
                      (Get (Object, Row, Col) = Item)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object : in out Surface;
      Item   : in     Style_Type) with
      Post   => (for all Row in 1 .. Object.Rows =>
                   (for all Col in 1 .. Object.Cols =>
                      (Get (Object, Row, Col) = Item)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object  : in out Surface;
      From    : in     Cursor_Type;
      To      : in     Cursor_Type;
      Process : not null access procedure (Item     : in out Style_Type;
                                           Position : in     Cursor_Type)) with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => Modified (Object),
      Global => null;

   procedure Fill (
      Object  : in out Surface;
      Process : not null access procedure (Item     : in out Style_Type;
                                           Position : in     Cursor_Type)) with
      Post   => Modified (Object),
      Global => null;

   -->> Colours <<--

   procedure Fill_Background (
      Object : in out Surface;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type;
      Item   : in     RGBA_Type) with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => (for all Row in From.Row .. To.Row =>
                   (for all Col in From.Col .. To.Col =>
                      (not Is_Indexed (Object, Row, Col)) and then
                      (Get_Background (Object, Row, Col) = Item)))
         and then Modified (Object),
      Global => null;

   procedure Fill_Foreground (
      Object : in out Surface;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type;
      Item   : in     RGBA_Type) with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => (for all Row in From.Row .. To.Row =>
                   (for all Col in From.Col .. To.Col =>
                      (not Is_Indexed (Object, Row, Col)) and then
                      (Get_Foreground (Object, Row, Col) = Item)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object     : in out Surface;
      From       : in     Cursor_Type;
      To         : in     Cursor_Type;
      Background : in     RGBA_Type;
      Foreground : in     RGBA_Type) with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => (for all Row in From.Row .. To.Row =>
                   (for all Col in From.Col .. To.Col =>
                      (not Is_Indexed (Object, Row, Col))              and then
                      (Get_Background (Object, Row, Col) = Background) and then
                      (Get_Foreground (Object, Row, Col) = Foreground)))
         and then Modified (Object),
      Global => null;

   procedure Fill_Background (
      Object : in out Surface;
      Item   : in     RGBA_Type) with
      Post   => (for all Row in 1 .. Object.Rows =>
                   (for all Col in 1 .. Object.Cols =>
                      (not Is_Indexed (Object, Row, Col)) and then
                      (Get_Background (Object, Row, Col) = Item)))
         and then Modified (Object),
      Global => null;

   procedure Fill_Foreground (
      Object : in out Surface;
      Item   : in     RGBA_Type) with
      Post   => (for all Row in 1 .. Object.Rows =>
                   (for all Col in 1 .. Object.Cols =>
                      (not Is_Indexed (Object, Row, Col)) and then
                      (Get_Foreground (Object, Row, Col) = Item)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object     : in out Surface;
      Background : in     RGBA_Type;
      Foreground : in     RGBA_Type) with
      Post   => (for all Row in 1 .. Object.Rows =>
                   (for all Col in 1 .. Object.Cols =>
                      (not Is_Indexed (Object, Row, Col))              and then
                      (Get_Background (Object, Row, Col) = Background) and then
                      (Get_Foreground (Object, Row, Col) = Foreground)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object  : in out Surface;
      From    : in     Cursor_Type;
      To      : in     Cursor_Type;
      Process : not null access procedure (Background : in out RGBA_Type;
                                           Foreground : in out RGBA_Type;
                                           Position   : in     Cursor_Type))
   with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => (for all Row in From.Row .. To.Row =>
                   (for all Col in From.Col .. To.Col =>
                      ((not Is_Indexed (Object, Row, Col))))),
      Global => null;

   procedure Fill (
      Object  : in out Surface;
      Process : not null access procedure (Background : in out RGBA_Type;
                                           Foreground : in out RGBA_Type;
                                           Position   : in     Cursor_Type))
   with
      Post   => (for all Row in 1 .. Object.Rows =>
                   (for all Col in 1 .. Object.Cols =>
                      ((not Is_Indexed (Object, Row, Col)))))
         and then Modified (Object),
      Global => null;

   -->> Palettes <<--

   procedure Fill_Background (
      Object : in out Surface;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type;
      Item   : in     Palette_Index) with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => (for all Row in From.Row .. To.Row =>
                   (for all Col in From.Col .. To.Col =>
                      (Is_Indexed (Object, Row, Col)) and then
                      (Get_Background (Object, Row, Col) = Item)))
         and then Modified (Object),
      Global => null;

   procedure Fill_Foreground (
      Object : in out Surface;
      From   : in     Cursor_Type;
      To     : in     Cursor_Type;
      Item   : in     Palette_Index) with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => (for all Row in From.Row .. To.Row =>
                   (for all Col in From.Col .. To.Col =>
                      (Is_Indexed (Object, Row, Col)) and then
                      (Get_Foreground (Object, Row, Col) = Item)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object     : in out Surface;
      From       : in     Cursor_Type;
      To         : in     Cursor_Type;
      Background : in     Palette_Index;
      Foreground : in     Palette_Index) with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => (for all Row in From.Row .. To.Row =>
                   (for all Col in From.Col .. To.Col =>
                      (Is_Indexed (Object, Row, Col))                  and then
                      (Get_Background (Object, Row, Col) = Background) and then
                      (Get_Foreground (Object, Row, Col) = Foreground)))
         and then Modified (Object),
      Global => null;

   procedure Fill_Background (
      Object : in out Surface;
      Item   : in     Palette_Index) with
      Post   => (for all Row in 1 .. Object.Rows =>
                   (for all Col in 1 .. Object.Cols =>
                      (Is_Indexed (Object, Row, Col)) and then
                      (Get_Background (Object, Row, Col) = Item)))
         and then Modified (Object),
      Global => null;

   procedure Fill_Foreground (
      Object : in out Surface;
      Item   : in     Palette_Index) with
      Post   => (for all Row in 1 .. Object.Rows =>
                   (for all Col in 1 .. Object.Cols =>
                      (Is_Indexed (Object, Row, Col)) and then
                      (Get_Foreground (Object, Row, Col) = Item)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object     : in out Surface;
      Background : in     Palette_Index;
      Foreground : in     Palette_Index) with
      Post   => (for all Row in 1 .. Object.Rows =>
                   (for all Col in 1 .. Object.Cols =>
                      (Is_Indexed (Object, Row, Col))                  and then
                      (Get_Background (Object, Row, Col) = Background) and then
                      (Get_Foreground (Object, Row, Col) = Foreground)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object  : in out Surface;
      From    : in     Cursor_Type;
      To      : in     Cursor_Type;
      Process : not null access procedure (Background : in out Palette_Index;
                                           Foreground : in out Palette_Index;
                                           Position   : in     Cursor_Type))
   with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => (for all Row in From.Row .. To.Row =>
                   (for all Col in From.Col .. To.Col =>
                      ((Is_Indexed (Object, Row, Col)))))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object  : in out Surface;
      Process : not null access procedure (Background : in out Palette_Index;
                                           Foreground : in out Palette_Index;
                                           Position   : in     Cursor_Type))
   with
      Post   => (for all Row in 1 .. Object.Rows =>
                   (for all Col in 1 .. Object.Cols =>
                      ((Is_Indexed (Object, Row, Col)))))
         and then Modified (Object),
      Global => null;

   -->> Omni <<--

   procedure Fill (
      Object     : in out Surface;
      From       : in     Cursor_Type;
      To         : in     Cursor_Type;
      Background : in     RGBA_Type;
      Foreground : in     RGBA_Type;
      Character  : in     Glyph;
      Style      : in     Style_Type) with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => (for all Row in From.Row .. To.Row =>
                   (for all Col in From.Col .. To.Col =>
                      (not Is_Indexed (Object, Row, Col))              and then
                      (Get_Background (Object, Row, Col) = Background) and then
                      (Get_Foreground (Object, Row, Col) = Foreground) and then
                      (Get (Object, Row, Col) = Character)             and then
                      (Get (Object, Row, Col) = Style)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object     : in out Surface;
      Background : in     RGBA_Type;
      Foreground : in     RGBA_Type;
      Character  : in     Glyph;
      Style      : in     Style_Type) with
      Post   => (for all Row in 1 .. Object.Rows =>
                   (for all Col in 1 .. Object.Cols =>
                      (not Is_Indexed (Object, Row, Col))              and then
                      (Get_Background (Object, Row, Col) = Background) and then
                      (Get_Foreground (Object, Row, Col) = Foreground) and then
                      (Get (Object, Row, Col) = Character)             and then
                      (Get (Object, Row, Col) = Style)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object     : in out Surface;
      From       : in     Cursor_Type;
      To         : in     Cursor_Type;
      Background : in     Palette_Index;
      Foreground : in     Palette_Index;
      Character  : in     Glyph;
      Style      : in     Style_Type) with
      Pre    => In_Range (Object, From, To) or else raise Constraint_Error,
      Post   => (for all Row in From.Row .. To.Row =>
                   (for all Col in From.Col .. To.Col =>
                      (not Is_Indexed (Object, Row, Col))              and then
                      (Get_Background (Object, Row, Col) = Background) and then
                      (Get_Foreground (Object, Row, Col) = Foreground) and then
                      (Get (Object, Row, Col) = Character)             and then
                      (Get (Object, Row, Col) = Style)))
         and then Modified (Object),
      Global => null;

   procedure Fill (
      Object     : in out Surface;
      Background : in     Palette_Index;
      Foreground : in     Palette_Index;
      Character  : in     Glyph;
      Style      : in     Style_Type) with
      Post   => (for all Row in 1 .. Object.Rows =>
                   (for all Col in 1 .. Object.Cols =>
                      (not Is_Indexed (Object, Row, Col))              and then
                      (Get_Background (Object, Row, Col) = Background) and then
                      (Get_Foreground (Object, Row, Col) = Foreground) and then
                      (Get (Object, Row, Col) = Character)             and then
                      (Get (Object, Row, Col) = Style)))
         and then Modified (Object),
      Global => null;

   --<<------>>--
   -->> Text <<--
   --<<------>>--

   procedure Put (
      Object : in out Surface;
      Row    : in     Row_Type;
      Col    : in     Col_Type;
      Item   : in     Glyph_String) with
      Pre      => (In_Range (Object, Row, Col) and then
                   In_Range (Object, Row, Col + Item'Length - 1))
                  or else raise Constraint_Error,
      Post     => (for all I in Col .. Col + Item'Length - 1 =>
                     (Get (Object, Row, Col)
                        = Item (Item'First + Natural (I - Col))))
         and then Modified (Object),
      Global   => null;

   -- TODO:
   -- * Put (String, Style);
   -- * Put (String, Foreground, Background);   (indexed and not)
   -- * Put (String, Style, Foreground, Background);  (indexed and not)
   -- * Put (Block);
   -- * Put (Block, Style);
   -- * Put (Block, Foreground, Background); (indexed and not)
   -- * Put (Block, Style, Foreground, Background);   (indexed and not)

   --<<---------->>--
   -->> Palettes <<--
   --<<---------->>--

   function Get_Palette (
      Object : aliased in Surface)
      return Palette_Constant_Reference_Type with
      Global => null;

   function Set_Palette (
      Object : aliased in out Surface)
      return Palette_Reference_Type with
      Post   => Modified (Object),
      Global => null;

private

   use Implementation;

   --<<--------->>--
   -->> Surface <<--
   --<<--------->>--

   type Surface (
      Rows : Positive_Row_Count;
      Cols : Positive_Col_Count) is
      tagged record
         Updated  : Boolean          := True;
         From     : Cursor_Type      := (1, 1);
         To       : Cursor_Type      := (Rows, Cols);
         Palette  : aliased Palettes.Palette_Type := Palettes.Default_Palette;
         Matrix   : Matrix_Type (1 .. Rows, 1 .. Cols)
                  := (others => (others => Default_Cell));
      end record;

   --<<------------------>>--
   -->> Helper Functions <<--
   --<<------------------>>--

   function Is_Indexed (
      Object : in Surface;
      Row    : in Row_Type;
      Col    : in Col_Type)
      return Boolean is (
      Object.Matrix (Row, Col).Has_Name);

   --<<-------------->>--
   -->> Surface Info <<--
   --<<-------------->>--

   function Modified (
      Object : in Surface)
      return Boolean is (
      Object.Updated);

   --<<-------------------->>--
   -->> On Cell Operations <<--
   --<<-------------------->>--

   -->> Glyphs <<--

   function Constant_Reference (
      Object : aliased in Surface;
      Row    :         in Positive_Row_Count;
      Col    :         in Positive_Col_Count)
      return Glyph_Constant_Reference_Type is (
      Element => Object.Matrix (Row, Col).Character'Access);

   function Get (
      Object : in Surface;
      Row    : in Positive_Row_Count;
      Col    : in Positive_Col_Count)
      return Glyph is (
      Object.Matrix (Row, Col).Character);

   -->> Styles <<--

   function Constant_Reference (
      Object : aliased in Surface;
      Row    :         in Positive_Row_Count;
      Col    :         in Positive_Col_Count)
      return Style_Constant_Reference_Type is (
      Element => Object.Matrix (Row, Col).Style'Access);

   function Get (
      Object : in Surface;
      Row    : in Positive_Row_Count;
      Col    : in Positive_Col_Count)
      return Style_Type is (
      Object.Matrix (Row, Col).Style);

   -->> Colours <<--

   function Get_Background (
      Object : in Surface;
      Row    : in Positive_Row_Count;
      Col    : in Positive_Col_Count)
      return RGBA_Type is (
      (if not Object.Is_Indexed (Row, Col)
         then Object.Matrix (Row, Col).Background
         else Object.Palette (Object.Matrix (Row, Col).Bg_Name)));

   function Get_Foreground (
      Object : in Surface;
      Row    : in Positive_Row_Count;
      Col    : in Positive_Col_Count)
      return RGBA_Type is (
      (if not Object.Is_Indexed (Row, Col)
         then Object.Matrix (Row, Col).Foreground
         else Object.Palette (Object.Matrix (Row, Col).Fg_Name)));

   -->> Palettes <<--

   function Get_Background (
      Object : in Surface;
      Row    : in Positive_Row_Count;
      Col    : in Positive_Col_Count)
      return Palette_Index is (
      (if Object.Is_Indexed (Row, Col)
         then Object.Matrix (Row, Col).Bg_Name
         else Palettes.Nearest (Object.Palette,
                                Object.Matrix (Row, Col).Background)));

   function Get_Foreground (
      Object : in Surface;
      Row    : in Positive_Row_Count;
      Col    : in Positive_Col_Count)
      return Palette_Index is  (
      (if Object.Is_Indexed (Row, Col)
         then Object.Matrix (Row, Col).Fg_Name
         else Palettes.Nearest (Object.Palette,
                                Object.Matrix (Row, Col).Foreground)));

   --<<---------->>--
   -->> Palettes <<--
   --<<---------->>--

   function Get_Palette (
      Object : aliased in Surface)
      return Palette_Constant_Reference_Type is (
      Element => Object.Palette'Access);

end Malef.Surfaces;
