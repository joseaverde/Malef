-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - E V E N T S . A D S                      --
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

package Malef.Events with Pure is

   type Key_Type is new Glyph;

   -- TODO: Define special keys such as F1, F2, ..., Arrows in the high part
   --       of the Wide_Wide_Character type. Since Unicode only has 2²¹
   --       available characters.

   type Mouse_Button is (Left_Button, Right_Button, Wheel);
   type Mouse_Action is (Move, Click, Wheel);
   type Mouse_Wheel is (Up, Down, Stay);

end Malef.Events;
