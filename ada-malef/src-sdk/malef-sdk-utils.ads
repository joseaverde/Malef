-------------------------------------------------------------------------------
--                                                                           --
--                   M A L E F - S D K - U T I L S . A D S                   --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
--                                                                           --
-------------------------------------------------------------------------------
--     Copyright (c) 2021 José Antonio Verde Jiménez All Rights Reserved     --
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

--
-- @summary
-- This package will help the development of widgets with certain actions
-- that are repeated such as writing in a widget with alignment.
--
package Malef.SDK.Utils is

   --
   -- This function puts a message with certain alignment inside a surface's
   -- bounds. If the surface bounds were too small and elipsis will be added
   -- as the last three characters of the surface.
   --
   procedure Put (Surface : in out Malef.Surfaces.Surface_Type;
      Message     : Str_Type;
      Row         : Row_Type;
      Row_Count   : Row_Type;
      Col         : Col_Type;
      Col_Count   : Col_Type;
      H_Alignment : Horizontal_Alignment_Type := Align_Left;
      V_Alignment : Vertical_Alignment_Type := Align_Top
   );

end Malef.SDK.Utils;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
