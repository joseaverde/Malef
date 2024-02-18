-------------------------------------------------------------------------------
--                                                                           --
--                       M A L E F - P L A T F O R M -                       --
--             G E N E R I C _ G E T _ I M M E D I A T E . A D B             --
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
-- with this program. If not, see <https://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with Interfaces;

procedure Malef.Platform.Generic_Get_Immediate (Item : out Glyph) is

   use Interfaces;

   Masks : constant array (0 .. 4) of Unsigned_8 := [
         2#0011_1111#, 2#0111_1111#, 2#0001_1111#, 2#0000_1111#, 2#0000_0111#];

   subtype UTF_8_1_Byte is Unsigned_8 range 2#0000_0000# .. 2#0111_1111#;
   subtype UTF_8_Part   is Unsigned_8 range 2#1000_0000# .. 2#1011_1111#;
   subtype UTF_8_2_Byte is Unsigned_8 range 2#1100_0000# .. 2#1101_1111#;
   subtype UTF_8_3_Byte is Unsigned_8 range 2#1110_0000# .. 2#1110_1111#;
   subtype UTF_8_4_Byte is Unsigned_8 range 2#1111_0000# .. 2#1111_0111#;

   Word : Unsigned_32 := 0;
   Byte : Unsigned_8;
   Mask : Unsigned_8;
   Size : Integer range 0 .. 4;
begin
   Get_Byte_Immediate (Byte);

   Size := (case Byte is when UTF_8_1_Byte => 1, when UTF_8_2_Byte => 2,
                         when UTF_8_3_Byte => 3, when UTF_8_4_Byte => 4,
                         when others => 0);
   Mask := Masks (Size);
   Word := Word or Unsigned_32 (Byte and Mask);
   Read_UTF_8_Sequence : for I in 2 .. Size loop
      Word := Shift_Left (Word, 6);
      Get_Byte_Immediate (Byte);
      if Byte not in UTF_8_Part then
         Word := 0;
         exit Read_UTF_8_Sequence;
      end if;
      Word := Word or Unsigned_32 (Byte and Masks (0));
   end loop Read_UTF_8_Sequence;

   Item := Wide_Wide_Character'Val (Word);
end Malef.Platform.Generic_Get_Immediate;
