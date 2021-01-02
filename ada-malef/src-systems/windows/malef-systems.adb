-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - S Y S T E M S . A D B                     --
--                             ( W I N D O W S )                             --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2021 José Antonio Verde Jiménez  All Rights Reserved  --
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
with Malef.Colors;

package body Malef.Systems is


   --====-----------------------------------====--
   --====-- INITIALIZATION / FINALIZATION --====--
   --====-----------------------------------====--

   procedure Initialize is
   begin

      -- We prepare some components from the library.
      -- We first set the `Has_Been_Initialized' variable to True while we
      -- call some functions that require Malef to be initialised.
      Has_Been_Initialized := True;

      -- TODO: Get the system's palette.
      Malef.Colors.Set_Palette(Malef.Colors.Windows_10_Console);

      -- We restore the previous state of the `Has_Been_Initialize' variable.
      Has_Been_Initialized := False;

   end Initialize;


   procedure Finalize is
   begin

      -- TODO: Share this function for all systems.
      -- We restore some library components to a previous state.
      Malef.Colors.Set_Palette(Malef.Colors.Malef_Palette);

   end Finalize;


   procedure Prepare_Terminal is
      procedure C_Driver_Prepare_Console;
      pragma Import (C,
                     C_Driver_Prepare_Console,
                     "_malef_setupConsole");
   begin

      C_Driver_Prepare_Console;

   end Prepare_Terminal;


   procedure Restore_Terminal is
      procedure C_Driver_Restore_Console;
      pragma Import (C,
                     C_Driver_Restore_Console,
                     "_malef_restoreConsole");
   begin

      C_Driver_Restore_Console;

   end Restore_Terminal;



   --====------------------------------====--
   --====-- TERMINAL/CONSOLE CONTROL --====--
   --====------------------------------====--


   function Get_Format (Format : Format_Type)
                        return String is
   begin

      -- TODO: Call external function.
      if Format = Default_Format then
         return "";
      end if;
      return "";

   end Get_Format;



   procedure Get_Terminal_Size (Rows : out Row_Type;
                                Cols : out Col_Type) is
      procedure C_Driver_Get_Console_Screen_Size(Rows: out Interfaces.C.short;
                                                 Cols: out Interfaces.C.short);
      pragma Import (C,
                     C_Driver_Get_Console_Screen_Size,
                     "_malef_getConsoleScreenSize");
      C_Rows, C_Cols : Interfaces.C.short;
   begin

      C_Driver_Get_Console_Screen_Size (Rows => C_Rows,
                                        Cols => C_Cols);

      Rows := Row_Type (C_Rows);
      Cols := Col_Type (C_Cols);


   end Get_Terminal_Size;


   procedure Set_Title (Name : String) is
      procedure C_Driver_Set_Console_Title (Title : Interfaces.C.Char_Array);
      pragma Import (C,
                     C_Driver_Set_Console_Title,
                     "_malef_setConsoleTitle");
   begin

      C_Driver_Set_Console_Title (Interfaces.C.To_C(Name));

   end Set_Title;


end Malef.Systems;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
