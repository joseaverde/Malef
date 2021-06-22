-------------------------------------------------------------------------------
--                                                                           --
--             M A L E F - S U B S Y S T E M S - A N S I . A D S             --
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

with Malef.Subsystems.Text_IO;

--
-- @summary
-- This package contains the functions for ANSI subsystem.
--
-- @description
-- This package contains a set of functions that will be dynamically loaded
-- using dynamic dispatching when the library is loaded. This functions will be
-- used by the System part of the library in order to use ANSI Escape Sequences
-- as the main way to control the terminal.
--
private package Malef.Subsystems.Ansi is

   type Subsystem is new Malef.Subsystems.Subsystem with null record;

   overriding
   procedure New_Page (Subsys : not null access Subsystem);
   overriding
   procedure Set_Title (Subsys : not null access Subsystem;
      Title  : String);

   overriding
   procedure Clear_Screen (Subsys : not null access Subsystem);
   overriding
   procedure Clear_Until_End_Of_Screen (Subsys : not null access Subsystem);
   overriding
   procedure Clear_Until_Start_Of_Screen (Subsys : not null access Subsystem);
   overriding
   procedure Clear_Entire_Screen (Subsys : not null access Subsystem);

   overriding
   procedure Clear_Current_Line (Subsys : not null access Subsystem);
   overriding
   procedure Clear_Until_End_Of_Line (Subsys : not null access Subsystem);
   overriding
   procedure Clear_Until_Start_Of_Line (Subsys : not null access Subsystem);
   overriding
   procedure Clear_Entire_Line (Subsys : not null access Subsystem);
   
   overriding
   procedure Enable_Line_Wrapping (Subsys : not null access Subsystem);
   overriding
   procedure Disable_Line_Wrapping (Subsys : not null access Subsystem);
   
   overriding
   procedure Make_Cursor_Visible (Subsys : not null access Subsystem);
   overriding
   procedure Make_Cursor_Invisible (Subsys : not null access Subsystem);
   
   overriding
   procedure Save_Screen (Subsys : not null access Subsystem);
   overriding
   procedure Restore_Screen (Subsys : not null access Subsystem);

   overriding
   procedure Put (Subsys : not null access Subsystem;
      Object : Shared_Surface_Access);


   function Get_Color_1 (
      Foreground : Color_Type;
      Background : Color_Type)
      return String
      with Pure_Function;
   function Get_Color_3 (
      Foreground : Color_Type;
      Background : Color_Type)
      return String
      with Pure_Function;
   function Get_Color_4 (
      Foreground : Color_Type;
      Background : Color_Type)
      return String
      with Pure_Function;
   function Get_Color_8 (
      Foreground : Color_Type;
      Background : Color_Type)
      return String
      with Pure_Function;
   function Get_Color_24 (
      Foreground : Color_Type;
      Background : Color_Type)
      return String
      with Pure_Function;

   function Get_Style (
      Style : Style_Array)
      return String
      with Pure_Function, Inline;

   function Get_Move (
      Row : Row_Type;
      Col : Col_Type)
      return String
      with Pure_Function, Inline;

   function Get_Format (
      Format : Format_Type)
      return String
      with Pure_Function, Inline;

   function Get_Clear
      return String
      with Pure_Function, Inline;

   Std_Out : Malef.Subsystems.Text_IO.Std_Out;

private

   type Get_Color_Function is not null access
      function (Foreground : Color_Type;
                Background : Color_Type)
                return String;
   Get_Color : Get_Color_Function := Get_Color_1'Access;

end Malef.Subsystems.Ansi;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
