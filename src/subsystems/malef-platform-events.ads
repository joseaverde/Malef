-------------------------------------------------------------------------------
--                                                                           --
--             M A L E F - P L A T F O R M - E V E N T S . A D S             --
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

with Ada.Containers.Bounded_Synchronized_Queues;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Malef.Events;

package Malef.Platform.Events is

   -->> Events <<--

   Max_Events : constant := 1024;

   package Event_Holders is
      new Ada.Containers.Indefinite_Holders (
      Element_Type => Malef.Events.Event_Type,
      "="          => Malef.Events."=");

   package Event_Queue_Interfaces is
      new Ada.Containers.Synchronized_Queue_Interfaces (
      Element_Type => Event_Holders.Holder);

   package Event_Queues is
      new Ada.Containers.Bounded_Synchronized_Queues (
      Queue_Interfaces => Event_Queue_Interfaces,
      Default_Capacity => Max_Events);

   function "+" (Right : in Malef.Events.Event_Type)
      return Event_Holders.Holder
      renames Event_Holders.To_Holder;

   Queue : aliased Event_Queues.Queue;

end Malef.Platform.Events;
