-------------------------------------------------------------------------------
--                                                                           --
--                    M A L E F - P A L E T T E S . A D B                    --
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

package body Malef.Palettes with Pure is

   function Distance (Left, Right : in RGBA_Type)
      return Integer is (
      [for I in Component_Name'Range =>
         (Integer (Left (I)) - Integer (Right (I))) ** 2]'Reduce ("+", 0));

   function Nearest (
      Palette : in Palette_Type;
      Item    : in RGBA_Type)
      return Palette_Index
   is
      Id   : Palette_Index := 0;
      Best : Integer := Distance (Palette (0), Item);
      Test : Integer;
   begin
      for I in Palette'Range loop
         Test := Distance (Palette (I), Item);
         if Test < Best then
            Best := Test;
            Id := I;
         end if;
      end loop;
      return Id;
   end Nearest;

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Palette_Type) is
   begin
      Buffer.Put ("[");
      for I in Palette_Index range 0 .. 7 loop
         Buffer.Put (" ");
         Put_Image (Buffer, Arg (I));
         Buffer.Put (",");
      end loop;
      Buffer.New_Line;
      Buffer.Put (" ");
      for I in Palette_Index range 8 .. 14 loop
         Buffer.Put (" ");
         Put_Image (Buffer, Arg (I));
         Buffer.Put (",");
      end loop;
      Buffer.Put (" ");
      Put_Image (Buffer, Arg (15));
      Buffer.Put ("]");
   end Put_Image;

end Malef.Palettes;
