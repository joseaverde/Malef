-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - L A B E L S . A D S                      --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   S P E C                              --
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

with Malef.Widgets;
with Malef.Surfaces;
private with Ada.Strings.Wide_Wide_Unbounded;
private with Ada.Containers.Vectors;

package Malef.Labels with Preelaborate is

   Indentation_Width : constant := 3;

   type Horizontal_Alignment is (Left_Aligned, Center_Aligned, Right_Aligned,
                                 Justified);
   type Vertical_Alignment is (Top_Aligned, Center_Aligned, Bottom_Aligned,
                               Justified);

   type Text_Direction is (Left_Right_Top_Bottom, Right_Left_Bottom_Top,
                           Top_Bottom_Right_Left);

   type Markup_Language is (Plain_Text, Markdown);

   type Label_Widget is
      new Widgets.Widget with
      private with
      Default_Initial_Condition =>
         Get_Horizontal_Alignment (Label_Widget) = Left_Aligned   and then
         Get_Vertical_Alignment (Label_Widget) = Top_Aligned      and then
         Get_Direction (Label_Widget) = Left_Right_Top_Bottom     and then
         Get_Text (Label_Widget) = "";

   function New_Label (
      Value      : in Wide_Wide_String;
      Markup     : in Markup_Language      := Plain_Text;
      Horizontal : in Horizontal_Alignment := Left_Aligned;
      Vertical   : in Vertical_Alignment   := Top_Aligned;
      Direction  : in Text_Direction       := Left_Right_Top_Bottom)
      return Label_Widget with
      Post     => Get_Text (New_Label'Result)                 = Value
         and then Get_Horizontal_Alignment (New_Label'Result) = Horizontal
         and then Get_Vertical_Alignment   (New_Label'Result) = Vertical
         and then Get_Direction (New_Label'Result)            = Direction,
      Global   => null;

   overriding
   procedure On_Draw (
      Widget  : in     Label_Widget;
      Surface : in out Surfaces.Surface;
      Area    : in     Widgets.Draw_Area);

   -->> Setters <<--

   procedure Set_Markup (
      Widget : in out Label_Widget;
      To     : in     Markup_Language) with
      Post   => Get_Markup (Widget) = To,
      Global => null;

   procedure Set_Horizontal_Alignment (
      Widget : in out Label_Widget;
      To     : in     Horizontal_Alignment) with
      Post   => Get_Horizontal_Alignment (Widget) = To,
      Global => null;

   procedure Set_Vertical_Alignment (
      Widget : in out Label_Widget;
      To     : in     Vertical_Alignment) with
      Post   => Get_Vertical_Alignment (Widget) = To,
      Global => null;

   procedure Set_Direction (
      Widget : in out Label_Widget;
      To     : in     Text_Direction) with
      Post   => Get_Direction (Widget) = To,
      Global => null;

   procedure Set_Text (
      Widget : in out Label_Widget;
      To     : in     Glyph_String) with
      Post   => Get_Text (Widget) = To,
      Global => null;

   -->> Getters <<--

   function Get_Markup (
      Widget : in Label_Widget)
      return Markup_Language with
      Global => null;

   function Get_Horizontal_Alignment (
      Widget : in Label_Widget)
      return Horizontal_Alignment with
      Global => null;

   function Get_Vertical_Alignment (
      Widget : in Label_Widget)
      return Vertical_Alignment with
      Global => null;

   function Get_Direction (
      Widget : in Label_Widget)
      return Text_Direction with
      Global => null;

   function Get_Text (
      Widget : in Label_Widget)
      return Glyph_String with
      Global => null;

private

   use Ada.Strings.Wide_Wide_Unbounded;

   type Label_Widget is
      new Widgets.Widget with
      record
         Markup     : Markup_Language        := Plain_Text;
         Horizontal : Horizontal_Alignment   := Left_Aligned;
         Vertical   : Vertical_Alignment     := Top_Aligned;
         Direction  : Text_Direction         := Left_Right_Top_Bottom;
         Value      : Unbounded_Wide_Wide_String;
      end record;

   function New_Label (
      Value      : in Wide_Wide_String;
      Markup     : in Markup_Language      := Plain_Text;
      Horizontal : in Horizontal_Alignment := Left_Aligned;
      Vertical   : in Vertical_Alignment   := Top_Aligned;
      Direction  : in Text_Direction       := Left_Right_Top_Bottom)
      return Label_Widget is (
      Widgets.Widget with
         Markup     => Markup,
         Horizontal => Horizontal,
         Vertical   => Vertical,
         Direction  => Direction,
         Value      => To_Unbounded_Wide_Wide_String (Value));

   -->> Getters <<--

   function Get_Markup (
      Widget : in Label_Widget)
      return Markup_Language is (
      Widget.Markup);

   function Get_Horizontal_Alignment (
      Widget : in Label_Widget)
      return Horizontal_Alignment is (
      Widget.Horizontal);

   function Get_Vertical_Alignment (
      Widget : in Label_Widget)
      return Vertical_Alignment is (
      Widget.Vertical);

   function Get_Direction (
      Widget : in Label_Widget)
      return Text_Direction is (
      Widget.Direction);

   function Get_Text (
      Widget : in Label_Widget)
      return Glyph_String is (
      To_Wide_Wide_String (Widget.Value));

   -->> Markups <<--

   type Glyph_Width is range 0 .. 2 with
      Size => 8;

   type Glyph_Kind is (Normal, Line_Break, Paragraph_Break, Indent, Deindent)
      with Size => 8;

   type Rich_Glyph is
      record
         Value      : Glyph;        -- 32 bits
         Kind       : Glyph_Kind;   -- 8 bits
         Width      : Glyph_Width;  -- 8 bits
         Style      : Style_Type;   -- 16 bits
         Background : RGBA_Type;    -- 32 bits
         Foreground : RGBA_Type;    -- 32 bits
      end record with
      Size => 128;

   package Rich_Glyph_Vectors is
      new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Rich_Glyph);

   subtype Rich_String is Rich_Glyph_Vectors.Vector;

   type Markup_Text is
      record
         Text : Rich_String;
      end record;

   type Parser_Type is
      not null access
      function (
         Item : in Unbounded_Wide_Wide_String)
         return Markup_Text;

end Malef.Labels;
