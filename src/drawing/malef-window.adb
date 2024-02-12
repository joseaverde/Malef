-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - W I N D O W . A D B                      --
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
-- with this program. If not, s://www.gnu.org/licenses/>.                    --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Task_Identification;
with Malef.Console_IO;
with Malef.Surfaces;
with Malef.System;

package body Malef.Window is

   procedure Show (
      Surface : in Surfaces.Surface;
      From    : in Cursor_Type;
      Size    : in Cursor_Type)
   is
      Iter_First : constant Cursor_Type := (
         Row => Row_Type'Max (1, -From.Row + 2),
         Col => Col_Type'Max (1, -From.Col + 2));
      First : constant Cursor_Type := (
         Row => Row_Type'Max (1, From.Row),
         Col => Col_Type'Max (1, From.Col));
      Last : constant Cursor_Type := (
         Row_Type'Min (Surface.Rows + First.Row - 1, Size.Row),
         Col_Type'Min (Surface.Cols + First.Col - 1, Size.Col));
      Iter_Last : constant Cursor_Type := (
         Row => Last.Row - First.Row + 1,
         Col => Last.Col - First.Col + 1);
      Row : Row_Type;
      Col : Col_Type;
   begin
      Malef.Console_IO.Begin_Frame;
      Row := First.Row;
      for Iter_Row in Iter_First.Row .. Iter_Last.Row loop
         Col := First.Col;
         for Iter_Col in Iter_First.Col .. Iter_Last.Col loop
            Malef.Console_IO.Put (
               Position   => (Row, Col),
               Item       => Surface (Iter_Row, Iter_Col),
               Background => Surface.Get_Background (Iter_Row, Iter_Col),
               Foreground => Surface.Get_Foreground (Iter_Row, Iter_Col),
               Style      => Surface (Iter_Row, Iter_Col));
            Col := Col + 1;
         end loop;
         Row := Row + 1;
      end loop;
      Malef.Console_IO.End_Frame;
      Malef.Console_IO.Flush;
   end Show;

   protected body Window is

      procedure Process_Group (
         Process : not null access
                   procedure (Object : aliased in out Groups.Group)) is
      begin
         Process.all (Group.Set_Group (1).Element.all);
      end Process_Group;

      procedure Set_Group (Object : in Groups.Group) is
      begin
         Group.Insert (1, Object);
      end Set_Group;

      procedure Display is
         In_Group renames Group.Set_Group (1).Element;
      begin
         In_Group.Update;
         Show (Group.Get_Group (1).See_Surface.Element.all,
               (In_Group.Row, In_Group.Col), (Rows, Cols));
      end Display;

      procedure Resize (
         New_Rows : in Positive_Row_Count;
         New_Cols : in Positive_Col_Count) is
      begin
         Rows := New_Rows;
         Cols := New_Cols;
      end Resize;

      procedure Redraw is null;

      -->> Callbacks <<--

      procedure Process (
         Event : in Events.Event_Type) is
      begin

         -- The Resize event is interesting for the Window. Also if there is
         -- no handler for the Kill, Cancel or Input_Closed events, by default
         -- we clean up and terminate the program.

         Ada.Text_IO.Put_Line (Event'Image);
         case Event.Name is
            when Malef.Events.Resize_Event => null;
            when Malef.Events.Cancel_Event
               | Malef.Events.Kill_Event
               | Malef.Events.Input_Closed =>
               Malef.System.Finalize;
               if Observers (Event.Name).Is_Empty then
                  Ada.Text_IO.Put_Line ("Program terminated");
               end if;
               -- It is intended to be blocking, this only happens when the
               -- program needs to be terminated.
               pragma Warnings (Off,
                  "potentially blocking operation in protected operation");
               Ada.Task_Identification.Abort_Task (
                  Ada.Task_Identification.Environment_Task);
               pragma Warnings (On,
                  "potentially blocking operation in protected operation");
            when others => null;
         end case;

         -- Call the observers.

         for Observer of Observers (Event.Name) loop
            Observer.Callback.all (Observer.Pointer.all, Event);
         end loop;

      end Process;

      procedure Register (
         Event    : in Events.Event_Name;
         Observer : not null access Event_Observer'Class;
         Callback : not null        Callback_Type) is
      begin
         Observers (Event).Append (
            Implementation.Observer_Info'(Observer, Callback));
      end Register;

      procedure Unregister (
         Event    : in Events.Event_Name;
         Observer : not null access Event_Observer'Class) is
      begin
         Observers (Event).Delete (Index => Observers (Event).Find_Index (
            Item => Implementation.Observer_Info'(Observer, null)));
      end Unregister;

   end Window;

   procedure Enqueue_Event (
      Event : in Events.Event_Type) is
   begin
      Malef.Console_IO.Queue.Enqueue (
         Malef.Console_IO.Event_Holders.To_Holder (Event));
   end Enqueue_Event;

   Rows : Positive_Row_Count;
   Cols : Positive_Col_Count;
begin
   Console_IO.Get_Dimensions (Rows, Cols);
   Console_IO.Register_Process (Window.Process'Access);
   Window.Resize (Rows, Cols);
end Malef.Window;
