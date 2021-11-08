-------------------------------------------------------------------------------
--                                                                           --
--                  M A L E F - C H A R A C T E R S . A D B                  --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
--                                                                           --
-------------------------------------------------------------------------------
--     Copyright (c) 2021 José Antonio Verde Jiménez All Rights Reserved     --
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

package body Malef.Characters is

   function To_UTF8 (Char : Char_Type)
      return UTF8_String is
   begin

      case Char is
         when 16#0000_0000# .. 16#0000_007F# =>
            return "" & Character'Val(Char);
         when 16#0000_0080# .. 16#0000_07FF# =>
            return
               Character'Val(2#1100_0000# or Interfaces.Shift_Right(Char, 6)) &
               Character'Val(2#1000_0000# or (Char and 2#0011_1111#));
         when 16#0000_0800# .. 16#0000_FFFF# =>
            return
               Character'Val(2#1110_0000# or Interfaces.Shift_Right(Char, 12))&
               Character'Val(2#1000_0000# or (Interfaces.Shift_Right(Char, 6)
                        and  2#0011_1111#)) &
               Character'Val(2#1000_0000# or (Char and 2#0011_1111#));
         when 16#0001_0000# .. 16#0010_FFFF# =>
            return
               Character'Val(2#1111_0000# or Interfaces.Shift_Right(Char, 24))&
               Character'Val(2#1100_0000# or (Interfaces.Shift_Right(Char, 12)
                        and  2#0011_1111#)) &
               Character'Val(2#1100_0000# or (Interfaces.Shift_Right(Char, 12)
                        and  2#0011_1111#)) &
               Character'Val(2#1000_0000# or (Char and 2#0011_1111#));
      end case;

   end To_UTF8;
 

   function To_UTF8 (Str : Str_Type)
      return UTF8_String
   is
      Result  : UTF8_String (1 .. UTF8_Length (Str));
      Current : Positive := 1;
   begin

      for Char of Str loop
         Get_Next:
            declare
               Next : constant UTF8_String := To_UTF8(Char);
            begin
               Result(Current .. Current+Next'Length-1) := Next;
               Current := Current + Next'Length;
            end Get_Next;
      end loop;

      return Result;

   end To_UTF8;
         

   function From_UTF8 (Str : UTF8_String)
      return Str_Type
   is
      function Calculate_Length return Natural
         with Inline is
      begin
         return Size : Natural := 0 do
            for Char of Str loop
               -- Each byte of a UTF8 character gives a hint of how many bytes
               -- will be used to encode it.
               -- We have the folowing cases:
               --  * 0xxx_xxxx   (This is plain ASCII, only 1 byte)
               --  * 110x_xxxx   (This means it takes 2 bytes)
               --  * 1110_xxxx   (This means it will take 3 bytes)
               --  * 1111_0xxx   (This means it will take 4 bytes)
               --
               -- However
               --  * 10xx_xxxx
               -- Are the bytes that follows one of each of the cases above,
               -- therefore we don't need to count them.
               -- if (Char_Type(Character'Pos(Char)) and 2#1000_0000#) = 0 then
               if Char_Type(Character'Pos(Char)) not in
                  2#10_000000# .. 2#10_111111#
               then
                  Size := Size + 1;
               end if;
            end loop;
         end return;
      end Calculate_Length;

      Current : Positive := 1;
      Buffer  : Char_Type;
      Bytes   : Natural := 1;
      Pos     : Char_Type;
   begin

      return Result : Str_Type (1 .. Calculate_Length)
      do
         for Char of Str loop
            Pos := Char_Type(Character'Pos(Char));
            if Pos not in 2#10_000000# .. 2#10_111111# then
               if Bytes /= 1 then
                  raise Constraint_Error with "Invalid UTF String!";
               end if;
            else
               if Bytes = 1 then
                  raise Constraint_Error with "Invalid UTF String";
               end if;
            end if;
            case Character'Pos(Char) is
               when 2#0_0000000# .. 2#0_1111111# =>   -- 0xxx_xxxx (1 byte)
                  Buffer := Char_Type (Character'Pos(Char));
               when 2#10_000000# .. 2#10_111111# =>   -- 10xx_xxxx (component)
                  Bytes := Bytes - 1;
                  Buffer := Buffer + Interfaces.Shift_Left(
                     Value  => Pos xor 2#10_000000#,
                     Amount => (Bytes - 1) * 6
                  );
               when 2#110_00000# .. 2#110_11111# =>   -- 110x_xxxx (2 bytes)
                  Buffer := Interfaces.Shift_Left(
                     Value  => Pos xor 2#110_00000#,
                     Amount => 6
                  );
                  Bytes := 2;
               when 2#1110_0000# .. 2#1110_1111# =>   -- 1110_xxxx (3 bytes)
                  Buffer := Interfaces.Shift_Left(
                     Value  => Pos xor 2#1110_0000#,
                     Amount => 12
                  );
                  Bytes := 3;
               when 2#11110_000# .. 2#11110_111# =>   -- 1111_0xxx (4 bytes)
                  Buffer := Interfaces.Shift_Left(
                     Value  => Pos xor 2#11110_000#,
                     Amount => 24
                  );
                  Bytes := 4;
               when others =>
                  null;
            end case;

            if Bytes = 1 then
               Result(Current) := Buffer;
               Current := Current + 1;
            end if;
         end loop;
      end return;

   end From_UTF8;


   function UTF8_Length (Str : Str_Type)
      return Natural
   is
      Size : Natural := 0;
   begin
      for Char of Str loop
         case Char is
            when 16#0000_0000# .. 16#0000_007F# => Size := Size + 1;
            when 16#0000_0080# .. 16#0000_07FF# => Size := Size + 2;
            when 16#0000_0800# .. 16#0000_FFFF# => Size := Size + 3;
            when 16#0001_0000# .. 16#0010_FFFF# => Size := Size + 4;
         end case;
      end loop;
      return Size;
   end UTF8_Length;




end Malef.Characters;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
