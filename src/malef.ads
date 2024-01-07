-------------------------------------------------------------------------------
--                                                                           --
--                             M A L E F . A D S                             --
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

package Malef with
   -- Global => in out synchronized
   Pure => True
is

   type Component_Name is (Red, Green, Blue, Alpha);
   type Component_Type is mod 256
      with Size => 8;
   type RGBA_Type is array (Component_Name'Range) of Component_Type
      with Size => 32;

   type Palette_Index is range 0 .. 15 with
      Size => 8;

   type Style_Name is (Bold,        Faint,       Italic,        Underline,
                       Slow_Blink,  Rapid_Blink, Reverse_Video, Conceal,
                       Crossed_Out, Doubly_Underline);

   type Style_Type is array (Style_Name) of Boolean with Pack;
   No_Style : constant Style_Type := (others => False);

   type Row_Type is range -32_768 .. 32_767;
   subtype Row_Count is Row_Type range 0 .. Row_Type'Last;
   subtype Positive_Row_Count is Row_Type range 0 .. Row_Type'Last;

   type Col_Type is range -32_768 .. 32_767;
   subtype Col_Count is Col_Type range 0 .. Col_Type'Last;
   subtype Positive_Col_Count is Col_Type range 0 .. Col_Type'Last;

   type Cursor_Type is
      record
         Row : Row_Type;
         Col : Col_Type;
      end record;

   subtype Glyph is Wide_Character;
   subtype Glyph_String is Wide_String;

   Nil : constant Glyph := Glyph'First;

   Initialization_Error : exception;

end Malef;
