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

with Malef.Exceptions;


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


   procedure New_Page is
   begin
      Loaded_Subsystems(Current_Subsystem).New_Page;
   end New_Page;

   procedure Set_Title (Name : String) is
   begin
      Loaded_Subsystems(Current_Subsystem).Set_Title (Name);
   end Set_Title;


   procedure Clear_Screen is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Screen;
   end Clear_Screen;

   procedure Clear_Until_End_Of_Screen is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Until_End_Of_Screen;
   end Clear_Until_End_Of_Screen;

   procedure Clear_Until_Start_Of_Screen is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Until_Start_Of_Screen;
   end Clear_Until_Start_Of_Screen;

   procedure Clear_Entire_Screen is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Entire_Screen;
   end Clear_Entire_Screen;


   procedure Clear_Current_Line is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Current_Line;
   end Clear_Current_Line;

   procedure Clear_Until_End_Of_Line is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Until_End_Of_Line;
   end Clear_Until_End_Of_Line;

   procedure Clear_Until_Start_Of_Line is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Until_Start_Of_Line;
   end Clear_Until_Start_Of_Line;

   procedure Clear_Entire_Line is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Entire_Line;
   end Clear_Entire_Line;


   procedure Enable_Line_Wrapping is
   begin
      Line_Wrapping := True;
      Loaded_Subsystems(Current_Subsystem).Enable_Line_Wrapping;
   end Enable_Line_Wrapping;

   procedure Disable_Line_Wrapping is
   begin
      Line_Wrapping := False;
      Loaded_Subsystems(Current_Subsystem).Disable_Line_Wrapping;
   end Disable_Line_Wrapping;

   function Has_Line_Wrapping return Boolean is
   begin
      return Line_Wrapping;
   end Has_Line_Wrapping;


   procedure Make_Cursor_Visible is
   begin
      Cursor_Visibility := True;
      Loaded_Subsystems(Current_Subsystem).Make_Cursor_Visible;
   end Make_Cursor_Visible;

   procedure Make_Cursor_Invisible is
   begin
      Cursor_Visibility := False;
      Loaded_Subsystems(Current_Subsystem).Make_Cursor_Invisible;
   end Make_Cursor_Invisible;

   function Is_Cursor_Visible return Boolean is
   begin
      return Cursor_Visibility;
   end Is_Cursor_Visible;


   procedure Save_Screen is
   begin
      Saved_Screen := True;
      Loaded_Subsystems(Current_Subsystem).Save_Screen;
   end Save_Screen;

   procedure Restore_Screen is
   begin
      if not Saved_Screen then
         raise Malef.Exceptions.Initialization_Error with
         "Cant restore non-saved screen!";
      end if;
      Loaded_Subsystems(Current_Subsystem).Restore_Screen;
   end Restore_Screen;

   function Has_Saved_Screen return Boolean is
   begin
      return Saved_Screen;
   end Has_Saved_Screen;

end Malef.Systems;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
