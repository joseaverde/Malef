-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - B U T T O N S . A D B                     --
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

package body Malef.Buttons is

   overriding
   procedure On_Draw (
      Object  : in     Button_Widget;
      Surface : in out Surfaces.Surface;
      Area    : in     Widgets.Draw_Area) is null;
   -- begin
   --    Malef.Widgets.Helpers.Draw_Square_Box (
   --    Surface (Area.From.Row, Area.From.Col)
   --       := Widgets.Square_Outline (Top_Left_Corner);
   --    Surface (Area.From.Row, Area.To.Col)
   --       := Widgets.Square_Outline (Top_Right_Corner);
   --    Surface (Area.To.Row, Area.From.Col)
   --       := Widgets.Square_Outline (Bottom_Left_Corner);
   --    Surface (Area.To.Row, Area.To.Col)
   --       := Widgets.Square_Outline (Bottom_Right_Corner);

   --    for Row in 1 .. Area.From.Row loop

   -- end On_Draw;

   procedure Set_Widget (
      Object : in out Button_Widget;
      Item   : in     Widgets.Widget'Class) is
   begin
      Object.Holder.Hold (Item);
   end Set_Widget;

end Malef.Buttons;
