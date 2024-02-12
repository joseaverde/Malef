-------------------------------------------------------------------------------
--                                                                           --
--           M A L E F - C O N S O L E _ I O - C O M M O N . A D S           --
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

with Ada.IO_Exceptions;
with Ada.Interrupts;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Task_Identification;

private package Malef.Console_IO.Common is

   package Unicode renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

   procedure Initialize_For_Task (
      Id : in Ada.Task_Identification.Task_Id);

   procedure Setup_Interrupts;

   procedure Clear_Interrupts;

   procedure Get_Immediate (Item : out Wide_Wide_Character);

   task type Event_Feeder (
      Process : not null access
                protected procedure (Event : in Events.Event_Type));

   procedure Start_Timer;

   function From_Start return Duration;

   Termination_Error : exception;
   Device_Error      : exception renames Ada.IO_Exceptions.Device_Error;
   End_Error         : exception renames Ada.IO_Exceptions.End_Error;

private

   type Interrupt_Name is (Resize_Id, Kill_Id, Cancel_Id);

   No_Id : constant Ada.Interrupts.Interrupt_ID
         := Ada.Interrupts.Interrupt_ID'Last;

   type Interrupt_Array is
      array (Interrupt_Name)
      of Ada.Interrupts.Interrupt_ID;

   function Get_Interrupts
      return Interrupt_Array;

end Malef.Console_IO.Common;
