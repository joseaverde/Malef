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

   -- This package declares the types used to handle events. It declares types
   -- for working with mouses. And uses the upper bounds of the
   -- Wide_Wide_Character type to represent special keys such as F1, F2, ...,
   -- Arrows (since Unicode only covers 2²¹ characters).

   type Key_Type is new Glyph;

   Last_Key_Pos : constant := Key_Type'Pos (Key_Type'Last);

   Key_Unknown : constant Key_Type := Key_Type'Val (Last_Key_Pos -  0);
   Key_Up      : constant Key_Type := Key_Type'Val (Last_Key_Pos -  1);
   Key_Down    : constant Key_Type := Key_Type'Val (Last_Key_Pos -  2);
   Key_Left    : constant Key_Type := Key_Type'Val (Last_Key_Pos -  3);
   Key_Right   : constant Key_Type := Key_Type'Val (Last_Key_Pos -  4);

   type Mouse_Button is (Left_Button, Right_Button, Wheel);
   type Mouse_Action is (Move, Click, Wheel);
   type Mouse_Wheel is (Up, Down, Stay);

   type Event_Name is (
      Resize_Event,
      Keyboard_Event,
      Mouse_Event,
      Cancel_Event,
      Kill_Event,
      Input_Closed);

   type Event_Type (
      Name : Event_Name) is
      record
         Time : Duration := 0.0;
         case Name is
            when Resize_Event   => New_Size : Cursor_Type;
            when Keyboard_Event => Key      : Events.Key_Type;
            when Mouse_Event    => Button   : Events.Mouse_Button;
                                   Action   : Events.Mouse_Action;
                                   Wheel    : Events.Mouse_Wheel;
                                   Position : Cursor_Type;
            when Cancel_Event   => null;
            when Kill_Event     => null;
            when Input_Closed   => null;
         end case;
      end record;

end Malef.Events;
