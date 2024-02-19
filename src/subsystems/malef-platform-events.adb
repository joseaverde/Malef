-------------------------------------------------------------------------------
--                                                                           --
--             M A L E F - P L A T F O R M - E V E N T S . A D B             --
--                                                                           --
--                                 M A L E F                                 --
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

with Ada.Unchecked_Deallocation;

package body Malef.Platform.Events is

   task type Event_Feeder;
   type Event_Feeder_Access is access Event_Feeder;

   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Event_Feeder,
      Name   => Event_Feeder_Access);

   Feeder  : Event_Feeder_Access := null;
   Process : Window_Queue_Process := null;

   procedure Register (Process : not null Window_Queue_Process) is
   begin
      if Events.Process /= null then
         raise Program_Error with "Attempting to tamper with proteceted " &
            "event handler for Malef.Platform.Events.Queue!!!";
      end if;
      Events.Process := Process;
   end Register;

   task body Event_Feeder is
      Holder : Event_Holders.Holder;
   begin
      loop
         Queue.Dequeue (Holder);
         Process.all (Holder.Constant_Reference.Element.all);
      end loop;
   end Event_Feeder;

   procedure Clear is
      use type Ada.Containers.Count_Type;
      Holder : Event_Holders.Holder;
   begin
      while Queue.Current_Use /= 0 loop
         select
            Queue.Dequeue (Holder);
         or
            delay 0.001;
         end select;
      end loop;
   end Clear;

   procedure Initialize is
   begin
      Feeder := new Event_Feeder;
   end Initialize;

   procedure Finalize is
   begin
      abort Feeder.all;
      Free (Feeder);
      Clear;
   end Finalize;

end Malef.Platform.Events;
