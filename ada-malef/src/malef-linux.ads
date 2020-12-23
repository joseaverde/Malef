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

end Malef.Linux;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
