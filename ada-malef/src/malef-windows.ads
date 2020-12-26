-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - W I N D O W S . A D S                     --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
--                                                                           --
-------------------------------------------------------------------------------
--     Copyright (c) 2020 José Antonio Verde Jiménez All Rights Reserved     --
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
--
--
-- @description
--
private package Malef.Windows is

   --
   -- This procedure prepares the console.
   --
   procedure Prepare_Console;

   --
   -- This procedure cleans up and restores the console.
   --
   procedure Restore_Console;


   --
   -- This function returns the number of columns and rows the console has got.
   -- It needs the Windows API to be available, thus in Linux it will always
   -- return 80x24 (the default Console size).
   --
   -- @param Rows
   -- The number of rows.
   --
   -- @param Cols
   -- The number of cols.
   --
   procedure Get_Console_Size (Rows : out Row_Type;
                                Cols : out Col_Type);

   --
   -- This procedure changes the console title.
   --
   -- @param Name
   -- The new Console name.
   --
   procedure Set_Title (Name : String);

end Malef.Windows;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
