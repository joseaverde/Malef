-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - W I N D O W . A D S                      --
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

with Malef.Boxes;
with Malef.Surfaces;

package Malef.Window is

   package Implementation is

      type Window_State is (Idle, Drawing, Resizing);

   end Implementation;

   protected Window is

      procedure Set_Box (Box : in Boxes.Box);

      procedure Resize (
         Rows : in Positive_Row_Count;
         Cols : in Positive_Col_Count);

      procedure Redraw;

   private

      Box : Boxes.Box (1);

   end Window;

   procedure Show (Surface : in Surfaces.Surface);

end Malef.Window;
