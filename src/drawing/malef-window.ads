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

   pragma Elaborate_Body;

   -- This package provides the protected type Window which represents the
   -- Terminal itself. It provides a system of callbacks that notify objects
   -- when an event occurs.
   --
   -- If you are using `Malef.Application` you should be modifying this package
   -- (if you don't know what you are doing). You can only register new
   -- callbacks.

   protected Window is

      procedure Process_Group (
         Process : not null access
                   procedure (Object : aliased in out Groups.Group));

      procedure Set_Group (Object : in Groups.Group);

      -- TODO: with move semantics. procedure Set_Group

      procedure Resize (
         New_Rows : in Positive_Row_Count;
         New_Cols : in Positive_Col_Count);

      procedure Get_Dimensions (
         Row_Count : out Positive_Row_Count;
         Col_Count : out Positive_Col_Count);

      procedure Display;

   private
      Group : Groups.Group (1);
      Rows  : Row_Type;
      Cols  : Col_Type;
   end Window;

   --<<----------->>--
   -->> Callbacks <<--
   --<<----------->>--

   type Event_Observer is limited interface;

   type Callback_Type is access
      protected procedure (
         Observer : aliased in out Event_Observer'Class;
         Event    :         in     Events.Event_Type);

   package Implementation is

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
         array (Events.Event_Name)
         of Observer_Vectors.Vector;

   end Implementation;

   protected Callbacks is

      procedure Process (
         Event : in Events.Event_Type);

      procedure Register (
         Event    : in Events.Event_Name;
         Observer : not null access Event_Observer'Class;
         Callback : not null        Callback_Type);

      procedure Unregister (
         Event    : in Events.Event_Name;
         Observer : not null access Event_Observer'Class);

   private
      Observers : Implementation.Observer_Vector_By_Event;
   end Callbacks;

   procedure Enqueue_Event (Event : in Events.Event_Type);

end Malef.Window;
