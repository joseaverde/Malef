-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - W I D G E T S . A D S                     --
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

with Malef.Surfaces;

package Malef.Widgets with Pure is

   type Widget is limited interface;

   type Draw_Area is
      record
         From : Cursor_Type;
         To   : Cursor_Type;
      end record;

   procedure On_Draw (
      Object  : in     Widget;
      Surface : in out Surfaces.Surface;
      Area    : in     Draw_Area) is
      abstract with
      Pre'Class => True;
   -- TODO: Check unchanged

   type Outline_Part is (
      Left_Side, Right_Side,           Top_Side, Bottom_Side,
      Top_Left_Corner,    Top_Intersection,    Top_Right_Corner,
      Left_Intersection,  Center_Intersection, Right_Intersection,
      Bottom_Left_Corner, Bottom_Intersection, Bottom_Right_Corner);

   type Outline_Type is array (Outline_Part) of Glyph;

   type Outline_Style is (Single, Double);

   Square_Outline : constant Outline_Type := "││──┌┬┐├┼┤└┴┘";
   Curvy_Outline  : constant Outline_Type := "││──╭┬╮├┼┤╰┴╯";
   Double_Outline : constant Outline_Type := "║║══╔╦╗╠╬╣╚╩╝";

end Malef.Widgets;
