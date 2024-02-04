-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - S Y S T E M . A D S                      --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   S P E C                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2024 José Antonio Verde Jiménez  All Rights Reserved  --
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

package Malef.System is

   -- This package is used to initialize and finalize the terminal. It also
   -- provides other primitives to work with them. You don't need to call
   -- `Finalize`, because it finalises even when there is a raised exception.
   -- If you use the Widget Toolkit and Applications you don't have to worry
   -- about using the `Initialize` function either.

   pragma Elaborate_Body;

   procedure Initialize;

   procedure Finalize;

   procedure Set_Title (
      Item : in Glyph_String);

end Malef.System;
