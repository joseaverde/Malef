-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - S Y S T E M S . A D B                     --
--                               ( L I N U X )                               --
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


package body Malef.Systems is

   --====-----------------------------------====--
   --====-- INITIALIZATION / FINALIZATION --====--
   --====-----------------------------------====--

   procedure Initialize is separate;
 
   procedure Finalize is separate;

   procedure Prepare_Terminal is separate;

   procedure Restore_Terminal is separate;


   --====------------------------------====--
   --====-- TERMINAL/CONSOLE CONTROL --====--
   --====------------------------------====--


   procedure Put (Object : Shared_Surface_Access) is
   begin

      Loaded_Subsystems(Current_Subsystem).Put(Object);

   end Put;

   -- IDEA: Make it inline, call the best function that can return the Format
   --       Use Dim/Bright styles if needed.
   function Get_Format (Format : Format_Type)
                        return String is
   begin

      return Loaded_Subsystems(Current_Subsystem).Get_Format(Format);

   end Get_Format;


   procedure Get_Terminal_Size (Rows : out Row_Type;
                                Cols : out Col_Type) is separate;

   procedure Set_Title (Name : String) is separate;

end Malef.Systems;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
