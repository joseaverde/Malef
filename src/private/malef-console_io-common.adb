-------------------------------------------------------------------------------
--                                                                           --
--           M A L E F - C O N S O L E _ I O - C O M M O N . A D B           --
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

with Ada.IO_Exceptions;
with Interfaces.C;
with Interfaces;
with System;

package body Malef.Console_IO.Common is

   -- The GNAT standard library has a bug in the Ada.Wide_Text_IO and
   -- Ada.Wide_Wide_Text_IO packages. Mainly when you use the
   --
   --    procedure Get_Immediate (
   --       Item      : out Wide_Wide_Character;
   --       Available : out Boolean)
   --
   -- One would expect that if there is no character in the input buffer, it
   -- will terminate and put Available to False. However, it is only
   -- implemented for the Ada.Text_IO package, not for the Wide variantes. So
   -- it waits for input and then sets Available to True.
   --
   -- If you read the source code for the body of any of those packages
   -- (a-ztextio.adb) there is comment:
   --
   --  «Shouldn't we use getc_immediate_nowait here, like Text_IO???»
   --
   -- I'm going to patch the bug and report it to GCC. But for the time being
   -- I will be writing the implementation here. It will be removed in future
   -- versions when the patch is available upstream.
   --
   -- This implementation only works for UTF-8 for the time being.

   subtype FILE is System.Address;

   procedure getc_immediate_nowait (
      stream      : in     FILE;
      ch          :    out Interfaces.C.int;
      end_of_file :    out Interfaces.C.int;
      avail       :    out Interfaces.C.int) with
      Import        => True,
      Convention    => C,
      External_Name => "getc_immediate_nowait";

   function ferror (
      stream : in FILE)
      return Interfaces.C.int with
      Import        => True,
      Convention    => C,
      External_Name => "ferror";

   stdin : aliased FILE with
      Import        => True,
      Convention    => C,
      External_Name => "stdin";

   procedure Get_Immediate_Step (
      Item      : out Interfaces.Unsigned_8;
      Available : out Boolean)
   is
      use type Interfaces.C.int;
      ch    : Interfaces.C.int;
      eof   : Interfaces.C.int;
      avail : Interfaces.C.int;
   begin
      getc_immediate_nowait (stdin, ch, eof, avail);
      Available := False;

      if ferror (stdin) /= 0 then
         raise Ada.IO_Exceptions.Device_Error;
      elsif eof /= 0 then
         raise Ada.IO_Exceptions.End_Error;
      elsif avail = 0 then
         Item := 0;
      else
         Available := True;
         Item := Interfaces.Unsigned_8 (ch);
      end if;
   end Get_Immediate_Step;

   use Interfaces;

   Masks : constant array (0 .. 4) of Unsigned_8 := [
         2#0011_1111#, 2#0111_1111#, 2#0001_1111#, 2#0000_1111#, 2#0000_0111#];

   subtype UTF_8_1_Byte is Unsigned_8 range 2#0000_0000# .. 2#0111_1111#;
   subtype UTF_8_Part   is Unsigned_8 range 2#1000_0000# .. 2#1011_1111#;
   subtype UTF_8_2_Byte is Unsigned_8 range 2#1100_0000# .. 2#1101_1111#;
   subtype UTF_8_3_Byte is Unsigned_8 range 2#1110_0000# .. 2#1110_1111#;
   subtype UTF_8_4_Byte is Unsigned_8 range 2#1111_0000# .. 2#1111_0111#;

   procedure Get_Immediate (
      Item      : out Wide_Wide_Character;
      Available : out Boolean)
   is
      Word : Unsigned_32 := 0;
      Byte : Unsigned_8;
      Mask : Unsigned_8;
      Size : Integer range 0 .. 4;
   begin
      Get_Immediate_Step (Byte, Available);

      if Available then
         Size := (case Byte is when UTF_8_1_Byte => 1, when UTF_8_2_Byte => 2,
                               when UTF_8_3_Byte => 3, when UTF_8_4_Byte => 4,
                               when others => 0);
         Mask := Masks (Size);
         Word := Word or Unsigned_32 (Byte and Mask);
         Read_UTF_8_Sequence : for I in 2 .. Size loop
            Word := Shift_Left (Word, 6);
            Get_Immediate_Step (Byte, Available);
            if not Available or else Byte not in UTF_8_Part then
               Word := 0;
               exit Read_UTF_8_Sequence;
            end if;
            Word := Word or Unsigned_32 (Byte and Masks (0));
         end loop Read_UTF_8_Sequence;

      end if;

      Item := Wide_Wide_Character'Val (Word);
   end Get_Immediate;

end Malef.Console_IO.Common;
