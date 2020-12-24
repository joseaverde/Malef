-------------------------------------------------------------------------------
--                                                                           --
--                       M A L E F - L I N U X . A D S                       --
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

with Ada.Interrupts;
with Ada.Interrupts.Names;

--
-- @summary
-- These are Linux specific functions.
--
-- @description
-- Functions or procedures that requiere a linux-only API will be implemented
-- separately so Windows system can have the Linux specification and
-- implementation.
--
private package Malef.Linux is

   pragma Elaborate_Body (Malef.Linux);

   --
   -- This procedure prepares the terminal.
   --
   -- @exception Malef.Exception.Initialization_Error
   -- This exception is raised if the terminal couldn't be prepared.
   --
   procedure Prepare_Terminal;

   --
   -- This procedure cleans up and restores the terminal.
   --
   -- @exception Malef.Exceptions.Initialization_Error
   -- This exception is raised if the terminal couldn't be restored.
   --
   procedure Restore_Terminal;

   --
   -- This function gets the full path of a programme similarly to `which'.
   --
   -- @return
   -- It returns the path without the last name, e.g:
   -- If you wanted to find `ls', then `/usr/bin' would be returned.
   -- In case nothing is found, "" is returned.
   --
   function Get_Path (Programme_Name : String)
                      return String;

   --
   -- This function returns the number of columns and rows a terminal has. It
   -- needs the Ioctl function to exist, thus in Windows it will always return
   -- 80x24.
   --
   procedure Get_Terminal_Size (Rows : out Row_Type;
                                Cols : out Col_Type);

   --
   -- This procedure changes the terminal title.
   --
   -- @param Name
   -- The name of the terminal.
   --
   procedure Set_Title (Name : String);

   -- This is the path for `stty', in case it doesn't exist the PATH would be
   -- `/stty'.
   Stty_PATH : constant String := Get_Path("stty") & '/' & "stty";

   -- Similarly to Stty_PATH, this is the PATH for `tput'.
   Tput_PATH : constant String := Get_Path("tput") & '/' & "tput";

   SIGWINCH : constant Ada.Interrupts.Interrupt_ID
            := Ada.Interrupts.Names.SIGWINCH;

end Malef.Linux;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
