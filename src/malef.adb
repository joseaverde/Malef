-------------------------------------------------------------------------------
--                                                                           --
--                             M A L E F . A D B                             --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   B O D Y                              --
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

with Ada.Characters.Conversions;

package body Malef is

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Component_Type)
   is
      Imgs : constant array (Component_Type range 0 .. 15) of Character
           := "0123456789ABCDEF";
   begin
      Buffer.Put (Imgs (Arg / 16) & Imgs (Arg mod 16));
   end Put_Image;

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     RGBA_Type) is
   begin
      Buffer.Put ("#");
      for Component of Arg loop
         Put_Image (Buffer, Component);
      end loop;
   end Put_Image;

   subtype Nibble is Component_Type range 0 .. 15;

   Hex_Values : constant array (Character'Range) of Nibble
              := ['0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4,
                  '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9,
                  'a' | 'A' => 10, 'b' | 'B' => 11, 'c' | 'C' => 12,
                  'd' | 'D' => 13, 'e' | 'E' => 14, 'f' | 'F' => 15,
                  others => 0];

   function Twice (Value : in Nibble)
      return Component_Type is (
      Component_Type (Value) * 16 + Value);

   function Combine (High, Low : in Nibble)
      return Component_Type is (
      Component_Type (High) * 16 + Low);

   function Hex (Item : in Wide_Wide_Character)
      return Nibble is (
      (if Item in Valid_Hexadecimal_Digit
         then Hex_Values (Ada.Characters.Conversions.To_Character (Item, '?'))
         else (raise Constraint_Error)));

   function Value (
      Item : in Wide_Wide_String)
      return RGBA_Type is (
      (case Item'Length is
         when 4 | 5 => [Twice (Hex (Item (Item'First + 1)))
                      , Twice (Hex (Item (Item'First + 2)))
                      , Twice (Hex (Item (Item'First + 3)))
                      , (if Item'Length = 4
                           then Component_Type'Last
                           else Twice (Hex (Item (Item'First + 4))))],
         when 7 | 9 => [Combine (Hex (Item (Item'First + 1)),
                                 Hex (Item (Item'First + 2)))
                      , Combine (Hex (Item (Item'First + 3)),
                                 Hex (Item (Item'First + 4)))
                      , Combine (Hex (Item (Item'First + 5)),
                                 Hex (Item (Item'First + 6)))
                      , (if Item'Length = 7
                           then Component_Type'Last
                           else Combine (Hex (Item (Item'First + 7)),
                                         Hex (Item (Item'First + 8))))],
         when others => (raise Constraint_Error)));

end Malef;
