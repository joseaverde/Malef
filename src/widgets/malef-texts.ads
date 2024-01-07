-------------------------------------------------------------------------------
--                                                                           --
--                       M A L E F - T E X T S . A D S                       --
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
private with Ada.Strings.Wide_Unbounded;

package Malef.Texts with Preelaborate is

   type Text_Alignment is (Left_Aligned, Center_Aligned,
                           Right_Aligned, Justified);

   type Text_Direction is (Left_Right_Top_Bottom, Right_Left_Bottom_Top,
                           Top_Bottom_Right_Left);

   type Text_Widget is
      new Widgets.Widget with
      private with
      Default_Initial_Condition =>
         Get_Alignment (Text_Widget) = Left_Aligned          and then
         Get_Direction (Text_Widget) = Left_Right_Top_Bottom and then
         Get_Text (Text_Widget) = "";

   overriding
   procedure On_Draw (
      Widget  : in     Text_Widget;
      Surface : in out Surfaces.Surface;
      Area    : in     Widgets.Draw_Area);

   function Get_Alignment (
      Widget : in Text_Widget)
      return Text_Alignment with
      Global => null;

   function Get_Direction (
      Widget : in Text_Widget)
      return Text_Direction with
      Global => null;

   function Get_Text (
      Widget : in Text_Widget)
      return Glyph_String with
      Global => null;

private

   type Text_Widget is
      new Widgets.Widget with
      record
         Alignment : Text_Alignment := Left_Aligned;
         Direction : Text_Direction := Left_Right_Top_Bottom;
         Value     : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      end record;

   function Get_Alignment (
      Widget : in Text_Widget)
      return Text_Alignment is (
      Widget.Alignment);

   function Get_Direction (
      Widget : in Text_Widget)
      return Text_Direction is (
      Widget.Direction);

   function Get_Text (
      Widget : in Text_Widget)
      return Glyph_String is (
      Ada.Strings.Wide_Unbounded.To_Wide_String (Widget.Value));

end Malef.Texts;
