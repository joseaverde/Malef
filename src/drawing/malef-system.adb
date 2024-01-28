-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - S Y S T E M . A D B                      --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   B O D Y                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2024 José Antonio Verde Jiménez  All Rights Reserved  --
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

with Ada.Finalization;
with Malef.Subsystem;
with Malef.Console_IO;

package body Malef.System is

   Initialised : Boolean := False;

   procedure Initialize is
   begin
      if Initialised then
         return;
      end if;
      Malef.Subsystem.Initialize;
      Malef.Console_IO.Initialize;
      Initialised := True;
   end Initialize;

   procedure Finalize is
   begin
      if not Initialised then
         return;
      end if;
      Malef.Console_IO.Finalize;
      Malef.Subsystem.Finalize;
      Initialised := False;
   end Finalize;

   type System_Handle is
      new Ada.Finalization.Limited_Controlled with
      null record;

   overriding
   procedure Finalize (Object : in out System_Handle) is
   begin
      if not Initialised then
         return;
      end if;
      Finalize;
   end Finalize;

end Malef.System;
