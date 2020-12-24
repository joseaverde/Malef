-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - E V E N T S . A D S                      --
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
-- This child package contains events and event handling.
--
-- @description
-- Events are functions and procedures that are executed every time a Signal
-- or Interrupt is raised.
--
package Malef.Events is

   --
   -- This protected `something' contains functions that are executed when a
   -- specific signal or interruption is raisd.
   --
   protected Event_Handler is

      --
      -- This procedure updates the terminal size. It also resizes the main
      -- surface. This procedure is raised automatically every time a signal
      -- SIGWINCH is raised.
      --
      procedure Update_Terminal_Size;
      pragma Interrupt_Handler (Update_Terminal_Size);
   end Event_Handler;

end Malef.Events;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
