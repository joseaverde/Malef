-------------------------------------------------------------------------------
--                                                                           --
--         M A L E F - P L A T F O R M - I N T E R R U P T S . A D S         --
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

with Ada.Interrupts;

package Malef.Platform.Interrupts is

   pragma Unreserve_All_Interrupts;

   procedure Attach;

   procedure Detach;

private

   type Interrupt_Name is (Resize_Id, Kill_Id, Cancel_Id);

   No_Id : constant Ada.Interrupts.Interrupt_ID
         := Ada.Interrupts.Interrupt_ID'Last;

   type Interrupt_Array is
      array (Interrupt_Name)
      of Ada.Interrupts.Interrupt_ID;

   function Get_Interrupts
      return Interrupt_Array;

end Malef.Platform.Interrupts;
