-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - W I N D O W . A D B                      --
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

with Malef.Console_IO;

package body Malef.Window is

   protected body Window is

      procedure Set_Box (Box : in Boxes.Box) is null;

      procedure Resize (
         Rows : in Positive_Row_Count;
         Cols : in Positive_Col_Count) is null;

      procedure Redraw is null;

   end Window;

   procedure Show (Surface : in Surfaces.Surface) is
   begin
      for Row in 1 .. Surface.Rows loop
         for Col in 1 .. Surface.Cols loop
            Malef.Console_IO.Console.Put (
               Position   => (Row, Col),
               Item       => Surface (Row, Col) & "",
               Background => Surface.Get_Background (Row, Col),
               Foreground => Surface.Get_Foreground (Row, Col),
               Style      => Surface (Row, Col));
         end loop;
      end loop;
      Malef.Console_IO.Console.Flush;
   end Show;

end Malef.Window;
