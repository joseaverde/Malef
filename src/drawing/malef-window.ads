-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - W I N D O W . A D S                      --
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

with Ada.Containers.Vectors;
with Malef.Groups;
with Malef.Events;

package Malef.Window is

   type Event_Observer is limited interface;

   type Event_Name is (
      Resize_Event,
      Keyboard_Event,
      Mouse_Event,
      Cancel_Event,
      Kill_Event);

   type Event_Type (
      Name : Event_Name) is
      record
         case Name is
            when Resize_Event   => New_Size : Cursor_Type;
            when Keyboard_Event => Key      : Events.Key_Type;
            when Mouse_Event    => Button   : Events.Mouse_Button;
                                   Action   : Events.Mouse_Action;
                                   Wheel    : Events.Mouse_Wheel;
                                   Position : Cursor_Type;
            when Cancel_Event   => null;
            when Kill_Event     => null;
         end case;
      end record;

   type Callback_Type is access
      procedure (Observer : aliased in out Event_Observer'Class;
                 Event    : in Event_Type);

   package Implementation is

      type Window_State is (Idle, Drawing, Resizing);

      type Observer_Access is access all Event_Observer'Class;

      type Observer_Info is
         record
            Pointer  : Observer_Access;
            Callback : Callback_Type;
         end record;

      overriding
      function "=" (Left, Right : in Observer_Info)
         return Boolean is (
         Left.Pointer = Right.Pointer);

      package Observer_Vectors is
         new Ada.Containers.Vectors (
         Index_Type   => Positive,
         Element_Type => Observer_Info);

      type Observer_Vector_By_Event is
         array (Event_Name)
         of Observer_Vectors.Vector;

   end Implementation;

   protected Window is

      procedure Process_Group (
         Process : not null access
                   procedure (Object : aliased in out Groups.Group));

      procedure Set_Group (Object : in Groups.Group);

      -- TODO: with move semantics. procedure Set_Group

      procedure Resize (
         Rows : in Positive_Row_Count;
         Cols : in Positive_Col_Count);

      procedure Display;

      procedure Redraw;

      -->> Callbacks <<--

      procedure Register (
         Event    : in Event_Name;
         Observer : not null access Event_Observer'Class;
         Callback : not null        Callback_Type);

      procedure Unregister (
         Event    : in Event_Name;
         Observer : not null access Event_Observer'Class);

   private

      Observers : Implementation.Observer_Vector_By_Event;
      Group     : Groups.Group (1);

   end Window;

end Malef.Window;
