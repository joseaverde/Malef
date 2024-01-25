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

package Malef.Labels with Preelaborate is

   type Text_Alignment is (Left_Aligned, Center_Aligned,
                           Right_Aligned, Justified);

   type Text_Direction is (Left_Right_Top_Bottom, Right_Left_Bottom_Top,
                           Top_Bottom_Right_Left);

   type Label_Widget is
      new Widgets.Widget with
      private with
      Default_Initial_Condition =>
         Get_Alignment (Label_Widget) = Left_Aligned          and then
         Get_Direction (Label_Widget) = Left_Right_Top_Bottom and then
         Get_Text (Label_Widget) = "";

   function New_Label (
      Value     : in Wide_Wide_String;
      Alignment : in Text_Alignment := Left_Aligned;
      Direction : in Text_Direction := Left_Right_Top_Bottom)
      return Label_Widget with
      Post     => Get_Text (New_Label'Result) = Value
         and then Get_Alignment (New_Label'Result) = Alignment
         and then Get_Direction (New_Label'Result) = Direction,
      Global   => null;

   overriding
   procedure On_Draw (
      Widget  : in     Label_Widget;
      Surface : in out Surfaces.Surface;
      Area    : in     Widgets.Draw_Area);

   -->> Setters <<--

   procedure Set_Alignment (
      Widget : in out Label_Widget;
      To     : in     Text_Alignment) with
      Post   => Get_Alignment (Widget) = To,
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

   function Get_Alignment (
      Widget : in Label_Widget)
      return Text_Alignment with
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
         Alignment : Text_Alignment := Left_Aligned;
         Direction : Text_Direction := Left_Right_Top_Bottom;
         Value     : Unbounded_Wide_Wide_String;
      end record;

   function New_Label (
      Value     : in Wide_Wide_String;
      Alignment : in Text_Alignment := Left_Aligned;
      Direction : in Text_Direction := Left_Right_Top_Bottom)
      return Label_Widget is (
      Widgets.Widget with
         Alignment => Alignment,
         Direction => Direction,
         Value     => To_Unbounded_Wide_Wide_String (Value));

   -->> Getters <<--

   function Get_Alignment (
      Widget : in Label_Widget)
      return Text_Alignment is (
      Widget.Alignment);

   function Get_Direction (
      Widget : in Label_Widget)
      return Text_Direction is (
      Widget.Direction);

   function Get_Text (
      Widget : in Label_Widget)
      return Glyph_String is (
      To_Wide_Wide_String (Widget.Value));

end Malef.Labels;
