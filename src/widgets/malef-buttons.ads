-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - B U T T O N S . A D S                     --
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
private with Malef.Widgets.Holders;

package Malef.Buttons with Preelaborate is

   type Button_Widget is
      new Widgets.Widget with
      private;

   function New_Button (
      Inside : in Widgets.Widget'Class)
      return Button_Widget with
      Global => null;

   overriding
   procedure On_Draw (
      Object  : in     Button_Widget;
      Surface : in out Surfaces.Surface;
      Area    : in     Widgets.Draw_Area);

   overriding
   function Name (
      Object : in Button_Widget)
      return Wide_Wide_String is (
      "Button");

   procedure Set_Widget (
      Object : in out Button_Widget;
      Item   : in     Widgets.Widget'Class);

private

   type Button_Widget is
      new Widgets.Widget with
      record
         Holder : Widgets.Holders.Holder;
      end record;

   function New_Button (
      Inside : in Widgets.Widget'Class)
      return Button_Widget is (
      Widgets.Widget with
      Holder => Widgets.Holders.Create (Inside));

end Malef.Buttons;
