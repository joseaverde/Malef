-------------------------------------------------------------------------------
--                                                                           --
--                  M A L E F - S U B S Y S T E M S . A D S                  --
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
-- These are the subsystems. Subsystems are runned inside systems such as
-- GNU/Linux, Windows or even a WebBrowser. These control the behaviour of how
-- everything is presented onto the screen.
--
-- @description
-- This package is created so Malef can be runned everywhere without needing
-- to include IO functions directly. I will also try to make this package
-- available to be loaded dynamically, that in systems like Windows that use
-- functions from the Windows API for CMD control, will differenciate between
-- running in an old CMD or not. There will be also an experimental support to
-- use ncurses itself for systems that can't use ANSI escape sequences, so in
-- Linux it will also be able to be dynamically loaded.
-- There are systems like GNU/Linux that can't use certain subsystems like the
-- Windows CMD.
--
private package Malef.Subsystems is

   type Subsystem is abstract tagged null record;
   type Subsystem_Access is access all Subsystem'Class;

   procedure New_Page (Subsys : not null access Subsystem)
      is abstract;
   procedure Set_Title (Subsys : not null access Subsystem;
                        Title  : String)
      is abstract;

   procedure Clear_Screen (Subsys : not null access Subsystem)
      is abstract;
   procedure Clear_Until_End_Of_Screen (Subsys : not null access Subsystem)
      is abstract;
   procedure Clear_Until_Start_Of_Screen (Subsys : not null access Subsystem)
      is abstract;
   procedure Clear_Entire_Screen (Subsys : not null access Subsystem)
      is abstract;

   procedure Clear_Current_Line (Subsys : not null access Subsystem)
      is abstract;
   procedure Clear_Until_End_Of_Line (Subsys : not null access Subsystem)
      is abstract;
   procedure Clear_Until_Start_Of_Line (Subsys : not null access Subsystem)
      is abstract;
   procedure Clear_Entire_Line (Subsys : not null access Subsystem)
      is abstract;

   procedure Enable_Line_Wrapping (Subsys : not null access Subsystem)
      is abstract;
   procedure Disable_Line_Wrapping (Subsys : not null access Subsystem)
      is abstract;

   procedure Make_Cursor_Visible (Subsys : not null access Subsystem)
      is abstract;
   procedure Make_Cursor_Invisible (Subsys : not null access Subsystem)
      is abstract;

   procedure Save_Screen (Subsys : not null access Subsystem)
      is abstract;
   procedure Restore_Screen (Subsys : not null access Subsystem)
      is abstract;

   -- We supose that the surface we want to print is the main surface.
   procedure Put (Subsys : not null access Subsystem;
                  Object : Shared_Surface_Access)
      is abstract;

package None is
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

   Subsystem_Handler : aliased Subsystem;
end None;


end Malef.Subsystems;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
