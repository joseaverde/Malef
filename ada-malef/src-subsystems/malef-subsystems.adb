-------------------------------------------------------------------------------
--                                                                           --
--                  M A L E F - S U B S Y S T E M S . A D B                  --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
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

with Malef.Exceptions;

package body Malef.Subsystems is

package body None is

   Error_Message : constant String :=
      "The Malef library hasn't been initialized yet!";

   overriding
   procedure New_Page (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end New_Page;

   overriding
   procedure Set_Title (Subsys : not null access Subsystem;
                        Title  : String) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Set_Title;


   overriding
   procedure Clear_Screen (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Clear_Screen;

   overriding
   procedure Clear_Until_End_Of_Screen (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Clear_Until_End_Of_Screen;

   overriding
   procedure Clear_Until_Start_Of_Screen (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Clear_Until_Start_Of_Screen;

   overriding
   procedure Clear_Entire_Screen (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Clear_Entire_Screen;


   overriding
   procedure Clear_Current_Line (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Clear_Current_Line;

   overriding
   procedure Clear_Until_End_Of_Line (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Clear_Until_End_Of_Line;

   overriding
   procedure Clear_Until_Start_Of_Line (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Clear_Until_Start_Of_Line;

   overriding
   procedure Clear_Entire_Line (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Clear_Entire_Line;
   

   overriding
   procedure Enable_Line_Wrapping (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Enable_Line_Wrapping;

   overriding
   procedure Disable_Line_Wrapping (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Disable_Line_Wrapping;
   

   overriding
   procedure Make_Cursor_Visible (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Make_Cursor_Visible;

   overriding
   procedure Make_Cursor_Invisible (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Make_Cursor_Invisible;
   

   overriding
   procedure Save_Screen (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Save_Screen;

   overriding
   procedure Restore_Screen (Subsys : not null access Subsystem) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Restore_Screen;


   overriding
   procedure Put (Subsys : not null access Subsystem;
                  Object : Shared_Surface_Access) is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   end Put;

   overriding
   function Get_Format (Subsys : not null access Subsystem;
                        Format : Format_Type)
                        return String is
   begin raise Malef.Exceptions.Initialization_Error with Error_Message;
   return "ERROR!";
   end Get_Format;

end None;



end Malef.Subsystems;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
