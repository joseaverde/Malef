-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - W I N D O W S . A D B                     --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
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

with Interfaces.C;

package body Malef.Windows is

   procedure Prepare_Console is
      procedure C_Driver_Prepare_Console;
      pragma Import (C,
                     C_Driver_Prepare_Console,
                     "_malef_setupConsole");
   begin

      C_Driver_Prepare_Console;

   end Prepare_Console;


   procedure Restore_Console IS
      procedure C_Driver_Restore_Console;
      pragma Import (C,
                     C_Driver_Restore_Console,
                     "_malef_restoreConsole");
   begin

      C_Driver_Restore_Console;

   end Restore_Console;


   procedure Get_Console_Size (Rows : out Row_Type;
                                Cols : out Col_Type) is
      procedure C_Driver_Get_Console_Screen_Size(rows: out Interfaces.C.short;
                                                 cols: out Interfaces.C.short);
      pragma Import (C,
                     C_Driver_Get_Console_Screen_Size,
                     "_malef_getConsoleScreenSize");

      c_rows, c_cols : Interfaces.C.short;
   begin

      C_Driver_Get_Console_Screen_Size (rows => c_rows,
                                        cols => c_cols);

      Rows := Row_Type (c_rows);
      Cols := Col_Type (c_cols);

   end Get_Console_Size;


   procedure Set_Title (Name : String) is
      procedure C_Driver_Set_Console_Title (Title : Interfaces.C.Char_Array);
      pragma Import (C,
                     C_Driver_Set_Console_Title,
                     "_malef_setConsoleTitle");
   begin

      C_Driver_Set_Console_Title(Interfaces.C.To_C(Name));

   end Set_Title;


end Malef.Windows;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
