-------------------------------------------------------------------------------
--                                                                           --
--    M A L E F - P L A T F O R M - T E R M I N A L - O U T P U T . A D S    --
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

package Malef.Platform.Terminal.Output is

   procedure Initialize;

   procedure Finalize;

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

end Malef.Platform.Terminal.Output;
