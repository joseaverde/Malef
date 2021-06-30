-------------------------------------------------------------------------------
--                                                                           --
--              M A L E F - S U B S Y S T E M S - C M D . A D S              --
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

private with Interfaces.C;
private with Malef.Colors;
with Malef.Subsystems.Components.Text_IO;

--
-- @summary
-- This package contains the functions to control the CMD subsystem.
--
-- @description
-- The CMD is meant to be runned on Windows because it requires the Windows
-- API to be available therefore making imposible the compilation targetting
-- a Windows system. This subsystem is peculiar because it requires to run
-- certain functions to change the different aspects of the console (Colours
-- and Styles).
--
private package Malef.Subsystems.CMD is

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


   Std_Out : Malef.Subsystems.Components.Text_IO.Std_Out;


private

   subtype WORD is Interfaces.Unsigned_16;

   ATTRIBUTE_ZERO           : constant := 16#0000#;
   FOREGROUND_INTENSITY     : constant := 16#0008#;
   BACKGROUND_INTENSITY     : constant := 16#0080#;
   COMMON_LVB_REVERSE_VIDEO : constant := 16#4000#;
   COMMON_LVB_UNDERSCORE    : constant := 16#8000#;

   use Malef.Colors;
   Windows_Conversion : constant array (Color_Kind'Range) of WORD := (
      Black    => 2#0000#,
      Red      => 2#0100#,
      Green    => 2#0010#,
      Yellow   => 2#0110#,
      Blue     => 2#0001#,
      Magenta  => 2#0101#,
      Cyan     => 2#0011#,
      White    => 2#0111#
   );

   procedure Set_Format (Format : Format_Type);
   procedure Set_Position (Position : Cursor_Type);

end Malef.Subsystems.CMD;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
