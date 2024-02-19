-------------------------------------------------------------------------------
--                                                                           --
--           M A L E F - P L A T F O R M - T E R M I N A L . A D B           --
--                                                                           --
--                                 M A L E F                                 --
--                                  A N S I                                  --
--                                                                           --
--                              A D A   B O D Y                              --
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

with Ada.Calendar;
with Malef.Platform.Terminal.Input;
with Malef.Platform.Terminal.Output;
with Malef.Platform.Interrupts;

package body Malef.Platform.Terminal is

   Start : Ada.Calendar.Time;

   procedure Initialize is
   begin
      Start := Ada.Calendar.Clock;
      Interrupts.Attach;
      Input.Initialize;
      Output.Initialize;
   end Initialize;

   procedure Finalize is
   begin
      Interrupts.Detach;
      Input.Finalize;
      Output.Finalize;
   end Finalize;

   function From_Start return Duration is (
      Ada.Calendar."-" (Ada.Calendar.Clock, Start));

end Malef.Platform.Terminal;
