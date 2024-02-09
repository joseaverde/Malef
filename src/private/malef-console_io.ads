-------------------------------------------------------------------------------
--                                                                           --
--                  M A L E F - C O N S O L E _ I O . A D S                  --
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

with Malef.Events;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;
with Ada.Containers.Indefinite_Holders;

private package Malef.Console_IO is

   -- WARNING: This package is used for internal purposes only. Don't use it
   --          directly. The A.P.I. is very volatile and is constantly changing
   --          and there is no guarantee that it if you use it directly there
   --          will be incompatibilities.
   --
   -- This package provides a common interface for the different subsystems for
   -- Input and Output. The subsystems are found in the `src/subsystems/<name>'
   -- directories and implement this body.
   --
   -- This package spec will change as I learn new techniques and learn new
   -- things about how terminal work.
   --
   -- The implementation may not be protected. As there only the Window can
   -- draw at the same time. Therefore we can avoid the performace penalty of
   -- protected type and keep everything outside and more simple.

   --<<---------------->>--
   -->> Initialization <<--
   --<<---------------->>--

   procedure Initialize;
   -- This procedure Initializes the terminal so that it can be used.
   -- This usually means:
   --  - Create a new screen for our application so we don't destroy what was
   --    before execution (adding as many lines as rows in the terminal is also
   --    possible).
   --  - Forbid echoing characters on the screen. If we want to show them, we
   --    do it manually.
   --  - The cursor by default is hidden.
   --  - If there is a mouse prepare it.

   procedure Finalize;
   -- This procedure Finalizes the terminal, i.e. restores the terminal to a
   -- previous state when Malef wasn't executing. This means, it undoes
   -- everything Initialize has changed.

   --<<-------->>--
   -->> Output <<--
   --<<-------->>--

   procedure Begin_Frame;
   -- This procedure tells the terminal that a new frame has started. In some
   -- places it is called to begin the synchronisation. What it does is
   -- stopping the terminal, and allowing writting a batch of characters at
   -- the same time. This avoids the tearing effect.
   --
   -- Calling multiple times Begin_Frame stacks them one over the other. That
   -- is, this subroutine ignores subsequent Begin_Frames until all of the
   -- frames have been ended.

   procedure End_Frame;
   -- This procedure tells the terminal when a frame has ended. It should be
   -- called after Begin_Frame. There is guarantee that the library won't
   -- end a frame that hasn't begun.
   --
   -- Also, we only end the frame, once all the opened frames are closed.

   procedure Put (
      Position   : in Cursor_Type;
      Item       : in Glyph_String;
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type);
   -- This procedure puts an string to the screen at a given position with
   -- the given attributes.

   procedure Put_Indexed (
      Position   : in Cursor_Type;
      Item       : in Glyph_String;
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type);
   -- This procedure puts an string to the screen at a given position with
   -- the given attributes.

   procedure Put (
      Position   : in Cursor_Type;
      Item       : in Glyph;
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type);
   -- This procedure puts an string to the screen at a given position with
   -- the given attributes.

   procedure Put_Indexed (
      Position   : in Cursor_Type;
      Item       : in Glyph;
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type);
   -- This procedure puts an string to the screen at a given position with
   -- the given attributes.

   procedure Flush;
   -- This procedure forces a flush of everything in any internal buffer to
   -- be outputed to the console.

   procedure Set_Title (
      Item : in Wide_Wide_String);
   -- This procedure changes the terminal's title.

   --<<------->>--
   -->> Input <<--
   --<<------->>--

   procedure Get_Dimensions (
      Rows : out Positive_Row_Count;
      Cols : out Positive_Col_Count);

   Max_Events : constant := 1024;

   package Event_Holders is
      new Ada.Containers.Indefinite_Holders (
      Element_Type                         => Events.Event_Type,
      "="                                  => Events."=");
      -- Max_Element_Size_in_Storage_Elements => Max_Event_Holders_Size);

   package Event_Queue_Interfaces is
      new Ada.Containers.Synchronized_Queue_Interfaces (
      Element_Type => Event_Holders.Holder);

   package Event_Queues is
      new Ada.Containers.Bounded_Synchronized_Queues (
      Queue_Interfaces => Event_Queue_Interfaces,
      Default_Capacity => Max_Events);

   Queue : aliased Event_Queues.Queue;

end Malef.Console_IO;
