-------------------------------------------------------------------------------
--                                                                           --
--                       M A L E F - T E X T S . A D B                       --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   B O D Y                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2021-2024 José Antonio Verde Jiménez  All Rights Reserved  --
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

with Ada.Wide_Wide_Characters.Handling;

package body Malef.Texts is

   function Visual_Width (
      Item : in Glyph)
      return Natural is (1);
   -- This function returns the width of a given character. This function isn't
   -- developed yet because it requires a long research. But the idea is that
   -- there are single width characters:
   --
   --    konnichiwa
   --    # # # # #
   --
   -- Double width characters:
   --
   --    こんにちは
   --    # # # # #
   --
   -- And there are characters that don't advance the cursor.
   --
   --    A` demain   (Imagine that 'A' + '`' compose 'À')
   --    #  # # #
   --
   -- This function returns how many cells does the cursor advance when one
   -- of those characters is printed.

   procedure Slice (
      Item : in     Unbounded_Wide_Wide_String;
      From : in out Positive;
      To   :    out Positive;
      EOL  :    out Boolean);
   -- This function slices a String into individual tokens. For instance if we
   -- want to write: "Hello, World!" & LF & "¡Hola, Mundo!"
   -- on the screen we need to separate words by spaces and new lines:
   --
   --    "Hello," "World!" LF "¡Hola," "Mundo!"
   --
   -- Then we can rearange them depending on the text direction and alignment.

   function Slice_Col_Count (
      Item : in Unbounded_Wide_Wide_String;
      From : in Positive;
      To   : in Natural)
      return Natural is (
      [for I in From .. To =>
         Visual_Width (Element (Item, I))]'Reduce ("+", 0));
   -- This function just applies the Visual_Width function to a string slice
   -- and returns the number of columns the string needs on the terminal to be
   -- rendered.

   -- generic
   --    Horizontal : in Boolean;
   -- procedure Generic_Put (
   --    Widget  : in     Text_Widget;
   --    Surface : in out Surfaces.Surface;
   --    Length  : in     Positive;
   --    From    : in     Positive;
   --    To      : in     Natural;
   --    Row     : in     Row_Type;
   --    Col     : in     Col_Type) with
   --    Pre    => To - From + 1 <= Length,
   --    Global => null;
   -- This generic procedure prints a string horizontally or vertically given
   -- the alignment of the widget. It expects a string whose range fits inside
   -- the surface.
   --
   -- @param Widget
   -- The widget associated with the message. It contains special metadata such
   -- as alignment that will be used later for formatting.
   --
   -- @param Surface
   -- The surface where the message will be printed.

   -- procedure Generic_Put (
   --    Widget  : in Text_Widget;
   --    Surface : in Surfaces.Surface;
   --    Length  : in Positive;
   --    From    : in Positive;
   --    To      : in Natural;
   --    Row     : in Row_Type;
   --    Col     : in Col_Type) is null;

   -- procedure Put_Row is new Generic_Put (Horizontal => True);
   -- procedure Put_Col is new Generic_Put (Horizontal => False);

   overriding
   procedure On_Draw (
      Widget  : in     Text_Widget;
      Surface : in out Surfaces.Surface;
      Area    : in     Widgets.Draw_Area) is
   begin
      Surface.Put (1, 1, "Hola");
   end On_Draw;

   -- overriding
   -- procedure On_Draw (
   --    Widget  : in     Text_Widget;
   --    Surface : in out Surfaces.Surface;
   --    Area    : in     Widgets.Draw_Area)
   -- is

   --    -- The first problem is that in English we right from left to right,from
   --    -- top to bottom. But in Arabic it is written from right to left, bottom
   --    -- to top. Or Japanese is sometimes written from top to bottom, right to
   --    -- left.
   --    --
   --    -- As most terminals have left to write by default we will make it our
   --    -- default too. We first have to check which sides are flipped. For
   --    -- instance, in Japanese it is flipped only the horizontal axis (cols).

   --    Cols_Flipped : constant Boolean :=
   --       Widget.Direction in Right_Left_Bottom_Top | Top_Bottom_Right_Left;
   --    Rows_Flipped : constant Boolean :=
   --       Widget.Direction in Right_Left_Bottom_Top;

   --    -- Now that we have that covered we can continue by flipping the given
   --    -- Draw Area, so that we can iterate without worrying about the
   --    -- direction. We will also keep the increment in each axis.

   --    Ranges : constant Widgets.Draw_Area := (
   --       From =>
   --          (Row => (if Rows_Flipped then Area.From.Row else Area.To.Row),
   --           Col => (if Cols_Flipped then Area.From.Col else Area.To.Col)),
   --       To   =>
   --          (Row => (if Rows_Flipped then Area.To.Row else Area.From.Row),
   --           Col => (if Cols_Flipped then Area.To.Col else Area.From.Col)));

   --    Iter_Row : constant Row_Type := (if Rows_Flipped then -1 else +1);
   --    Iter_Col : constant Col_Type := (if Cols_Flipped then -1 else +1);

   --    -- Then, we have to check if we are writing rows or columns first.
   --    -- In Japanese lines are vertical. But in English and Arabic, they are
   --    -- horizontal.

   --    Horizontal : constant Boolean :=
   --       Widget.Direction in Left_Right_Top_Bottom | Right_Left_Bottom_Top;

   --    -- We will also calculate, supposing we write in the previous fashion.
   --    -- How many lines do we have and how long is each line.

   --    Line_Length : constant Positive :=
   --       (if Horizontal then Positive (Area.To.Col - Area.From.Col + 1)
   --                      else Positive (Area.To.Row - Area.From.Row + 1));
   --    Line_Count : constant Positive :=
   --       (if Horizontal then Positive (Area.To.Row - Area.From.Row + 1)
   --                      else Positive (Area.To.Col - Area.From.Col + 1));

   --    First_Line : constant Positive :=
   --       (if Horizontal then Positive (Ranges.From.Row)
   --                      else Positive (Ranges.From.Col));
   --    Last_Line : constant Natural :=
   --       (if Horizontal then Natural (Ranges.To.Row)
   --                      else Natural (Ranges.To.Col));
   --    Line_Increment : constant Integer := (
   --       (if Horizontal then Integer (Iter_Row) else Integer (Iter_Col)));

   --    -- Finally we check which is the first index in the line.

   --    First_Index : constant Positive :=
   --       (if Horizontal then Positive (Ranges.From.Col)
   --                      else Positive (Ranges.From.Row));

   --    -- The idea is simple, read a line so that it fits on the box.
   --    -- Then draw it.

   --    Line : Natural := First_Line;
   --    From : Positive := 1;
   --    To   : Positive;

   -- begin

   --    -- We split it into two operations so we can avoid branching as much as
   --    -- possible.

   --    if Horizontal then

   --       loop
   --          exit when Line_Increment * Line > Line_Increment * Last_Line;
   --          Read_Line (Widget.Value, Line_Size, From, To);
   --          Put_Row (
   --             Widget  => Widget,
   --             Surface => Surface,
   --             Length  => Line_Size,
   --             From    => From,
   --             To      => To,
   --             Row     => Row_Type (Line),
   --             Col     => Col_Type (First_Index));
   --          Line := Line + Line_Increment;
   --          From := To + 1;
   --       end loop;

   --    else

   --       loop
   --          exit when Line_Increment * Line > Line_Increment * Last_Line;
   --          Read_Line (Widget.Value, Line_Size, From, To);
   --          Put_Col (
   --             Widget  => Widget,
   --             Surface => Surface,
   --             Length  => Line_Size,
   --             From    => From,
   --             To      => To,
   --             Row     => Row_Type (First_Index),
   --             Col     => Col_Type (Line));
   --          Line := Line + Line_Increment;
   --          From := To + 1;
   --       end loop;

   --    end if;

   -- end On_Draw;

   procedure Slice (
      Item : in     Unbounded_Wide_Wide_String;
      From : in out Positive;
      To   :    out Positive;
      EOL  :    out Boolean)
   is
      use Ada.Wide_Wide_Characters.Handling;
      LF : constant Glyph := Glyph'Val (Character'Pos (ASCII.LF));
      CR : constant Glyph := Glyph'Val (Character'Pos (ASCII.CR));
   begin

      -- Skip trailing spaces

      while From <= Length (Item) and then Is_Space (Element (Item, From)) loop
         From := From + 1;
      end loop;

      -- Read until space or line terminator

      To := From;
      while To <= Length (Item)
         and then not Is_Space (Element (Item, To))
         and then not Is_Line_Terminator (Element (Item, To))
      loop
         To := To + 1;
      end loop;

      -- Line terminator

      EOL := False;
      if To <= Length (Item)
         and then Is_Line_Terminator (Element (Item, To))
      then

         -- It ends with a terminator

         if To /= From then
            To := To - 1;

         -- We check for CRLF combinations and consider them as one.

         elsif Element (Item, To) = CR
            and then To < Length (Item)
            and then Element (Item, To + 1) = LF
         then
            To := To + 1;
            EOL := True;
         else
            EOL := True;
         end if;

      end if;

   end Slice;

   -->> Setters <<--

   procedure Set_Alignment (
      Widget : in out Text_Widget;
      To     : in     Text_Alignment) is
   begin
      Widget.Alignment := To;
   end Set_Alignment;

   procedure Set_Direction (
      Widget : in out Text_Widget;
      To     : in     Text_Direction) is
   begin
      Widget.Direction := To;
   end Set_Direction;

   procedure Set_Text (
      Widget : in out Text_Widget;
      To     : in     Glyph_String) is
   begin
      Widget.Value := To_Unbounded_Wide_Wide_String (To);
   end Set_Text;

end Malef.Texts;
