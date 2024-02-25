-------------------------------------------------------------------------------
--                                                                           --
--                       M A L E F - E M P T Y . A D S                       --
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

package Malef.Empty with Preelaborate is

   type Empty_Widget is new Widgets.Widget with null record;

   overriding
   procedure On_Draw (
      Object  : in     Empty_Widget;
      Surface : in out Surfaces.Surface;
      Area    : in     Widgets.Draw_Area) is null;

   overriding
   function Name (
      Object : in Empty_Widget)
      return Wide_Wide_String is (
      "Empty");

end Malef.Empty;
