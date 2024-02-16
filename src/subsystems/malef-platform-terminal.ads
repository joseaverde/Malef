-------------------------------------------------------------------------------
--                                                                           --
--           M A L E F - P L A T F O R M - T E R M I N A L . A D S           --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   S P E C                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2021-2024 José Antonio Verde Jiménez  All Rights Reserved  --
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

package Malef.Platform.Terminal is

   -- WARNING: This package is used for internal purposes only. Don't use it
   --          directly. The A.P.I. is very volatile and is constantly changing
   --          and there is no guarantee that it if you use it directly there
   --          will be incompatibilities.
   --
   -- This package provides a common interface for the different subsystems for
   -- Input and Output. The subsystems are found in the `src/subsystems/<name>'
   -- directories and implement this body.
   --
   -- This package spec will change as I learn new techniques and learn new
   -- things about how terminal work.
   --
   -- The implementation may not be protected. As there only the Window can
   -- draw at the same time. Therefore we can avoid the performace penalty of
   -- protected type and keep everything outside and more simple.

   procedure Initialize;
   -- This procedure Initializes the terminal so that it can be used.
   -- This usually means:
   --  - Create a new screen for our application so we don't destroy what was
   --    before execution (adding as many lines as rows in the terminal is also
   --    possible).
   --  - Forbid echoing characters on the screen. If we want to show them, we
   --    do it manually.
   --  - The cursor by default is hidden.
   --  - If there is a mouse prepare it.
   --  - Key presses don't echo.

   procedure Finalize;
   -- This procedure Finalizes the terminal, i.e. restores the terminal to a
   -- previous state when Malef wasn't executing. This means, it undoes
   -- everything Initialize has changed.

end Malef.Platform.Terminal;
