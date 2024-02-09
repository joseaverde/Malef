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

with Malef.Labels.Plain_Text_Parser;
with Malef.Labels.Markdown_Parser;

package body Malef.Labels is

   Parsers : constant array (Markup_Language) of Parser_Type
      := (Plain_Text => Plain_Text_Parser'Access
        , Markdown   => Markdown_Parser'Access
        );

   -- type Natural_Array is array (Positive range <>) of Natural;

   function Count_Lines (
      Text        : in Markup_Text;
      Line_Count  : in Natural;
      Line_Length : in Natural)
      return Natural with
      Post => Count_Lines'Result <= Line_Count;
   -- This function counts the lines.

   -- function Line_Lengths (
   --    Text        : in Natural;
   --    Line_Count  : in Natural;
   --    Line_Length : in Natural)
   --    return Natural;

   overriding
   procedure On_Draw (
      Widget  : in     Label_Widget;
      Surface : in out Surfaces.Surface;
      Area    : in     Widgets.Draw_Area)
   is

      -- The first problem is that there are languages where we write
      -- horizontally and others vertically. We want to count the number of
      -- lines there are and what is the length of each of those lines.

      Horizontal : constant Boolean :=
         (case Widget.Direction is
            when Left_Right_Top_Bottom
               | Right_Left_Bottom_Top => True,
            when Top_Bottom_Right_Left => False);

      Line_Count : constant Natural := Natural'Max (0,
         (if Horizontal
            then Integer (Area.To.Row - Area.From.Row + 1)
            else Integer (Area.To.Col - Area.From.Col + 1) / 2));
      Line_Length : constant Natural := Natural'Max (0,
         (if Horizontal
            then Integer (Area.To.Col - Area.From.Col + 1)
            else Integer (Area.To.Row - Area.From.Row + 1)));

      -- Then generate the Markup Text from the internal contents. This one
      -- has the information about the text preprocessed. So it easier to
      -- parse. We will have to count the number of lines too. We pass the
      -- Line_Count too so that it doesn't have to parse the whole text.

      Rich   : constant Markup_Text := Parsers (Widget.Markup) (Widget.Value);
      Lines  : constant Natural := Count_Lines (Rich, Line_Count, Line_Length);

      Index       : Positive := 1;
      Length      : Natural  := 0;
      Indentation : Natural  := 0;

   begin

      -- Horizontal first, to see whether it works

      for Line in 0 .. Lines - 1 loop
         Length := Indentation;
         while Index <= Rich.Text.Last_Index and then Length < Line_Length loop
            case Rich.Text (Index).Kind is
               when Normal =>
                  Surface.Set (
                     Row  => Row_Type (Line) + Area.From.Row,
                     Col  => Col_Type (Length) + Area.From.Col,
                     Item => Rich.Text (Index).Value);
                  if Rich.Text (Index).Width /= 1
                     and then Length < Line_Length - 1
                  then
                     Length := Length + 1;
                     Surface.Set (
                        Row  => Row_Type (Line) + Area.From.Row,
                        Col  => Col_Type (Length) + Area.From.Col,
                        Item => (if Rich.Text (Index).Width = 0
                                    then Bck else Dbl));
                  end if;
                  Length := Length + 1;
               when Line_Break =>
                  Length := Line_Length + 1;
               when Paragraph_Break =>
                  Length := Line_Length + 1;    -- TODO Skip one line
               when Indent =>
                  Indentation := Indentation + Indentation_Width;
               when Deindent =>
                  Indentation := Natural'Max (0, @ - Indentation_Width);
            end case;
            Index := Index + 1;
         end loop;
      end loop;

   end On_Draw;

   -- overriding
   -- procedure On_Draw (
   --    Widget  : in     Label_Widget;
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

   function Count_Lines (
      Text        : in Markup_Text;
      Line_Count  : in Natural;
      Line_Length : in Natural)
      return Natural
   is
      Length : Natural := 0;
      Count  : Natural := 1;
      Indent : Natural := 0;
   begin

      -- There is not enough space to write.

      if Line_Count = 0 or else Line_Length = 0 then
         return 0;
      end if;

      -- For each line calculate the length.

      for Char of Text.Text loop
         exit when Count = Line_Count;
         case Char.Kind is
            when Normal =>
               if Natural (Char.Width) + Length > Line_Length then
                  Count := Count + 1;
                  Length := Natural (Char.Width) + Indentation_Width * Indent;
               else
                  Length := Length + Natural (Char.Width);
               end if;
            when Line_Break =>
               Count := Count + 1;
               Length := Indentation_Width * Indent;
            when Paragraph_Break =>
               Count := Count + 2;
               Length := Indentation_Width * Indent;
            when Labels.Indent =>
               Indent := Indent + 1;
            when Deindent =>
               Indent := Natural'Max (0, Indent - 1);
         end case;
      end loop;

      return Natural'Min (Line_Count, Count);

   end Count_Lines;

   -->> Setters <<--

   -- TODO: Set Widget to be updated

   procedure Set_Markup (
      Widget : in out Label_Widget;
      To     : in     Markup_Language) is
   begin
      Widget.Markup := To;
   end Set_Markup;

   procedure Set_Horizontal_Alignment (
      Widget : in out Label_Widget;
      To     : in     Horizontal_Alignment) is
   begin
      Widget.Horizontal := To;
   end Set_Horizontal_Alignment;

   procedure Set_Vertical_Alignment (
      Widget : in out Label_Widget;
      To     : in     Vertical_Alignment) is
   begin
      Widget.Vertical := To;
   end Set_Vertical_Alignment;

   procedure Set_Direction (
      Widget : in out Label_Widget;
      To     : in     Text_Direction) is
   begin
      Widget.Direction := To;
   end Set_Direction;

   procedure Set_Text (
      Widget : in out Label_Widget;
      To     : in     Glyph_String) is
   begin
      Widget.Value := To_Unbounded_Wide_Wide_String (To);
   end Set_Text;

end Malef.Labels;
