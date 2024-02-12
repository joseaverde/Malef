-------------------------------------------------------------------------------
--                                                                           --
--                 MALEF-CONSOLE_IO-COMMON-GET_INTERRUPTS.ADB                --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              S E P A R A T E                              --
--                                 L I N U X                                 --
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

with Ada.Interrupts.Names;

separate (Malef.Console_IO.Common)
   function Get_Interrupts
      return Interrupt_Array is
   begin
      return [Resize_Id => No_Id
            , Cancel_Id => Ada.Interrupts.Names.SIGINT
            , Kill_Id   => Ada.Interrupts.Names.SIGTERM
            ];
   end Get_Interrupts;
