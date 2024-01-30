-------------------------------------------------------------------------------
--                                                                           --
--                    M A L E F - P A L E T T E S . A D S                    --
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

package Malef.Palettes with Pure is

   type Palette_Type is array (Palette_Index) of RGBA_Type with
      Put_Image => Put_Image;

   Black   : constant Palette_Index := 0;
   Red     : constant Palette_Index := 1;
   Green   : constant Palette_Index := 2;
   Yellow  : constant Palette_Index := 3;
   Blue    : constant Palette_Index := 4;
   Magenta : constant Palette_Index := 5;
   Cyan    : constant Palette_Index := 6;
   White   : constant Palette_Index := 7;

   Light_Black   : constant Palette_Index := 8 + 0;
   Light_Red     : constant Palette_Index := 8 + 1;
   Light_Green   : constant Palette_Index := 8 + 2;
   Light_Yellow  : constant Palette_Index := 8 + 3;
   Light_Blue    : constant Palette_Index := 8 + 4;
   Light_Magenta : constant Palette_Index := 8 + 5;
   Light_Cyan    : constant Palette_Index := 8 + 6;
   Light_White   : constant Palette_Index := 8 + 7;

   Default_Text_Foreground         : constant Palette_Index := Black;
   Default_Selected_Background     : constant Palette_Index := Red;
   Default_Window_Background       : constant Palette_Index := Green;
   Default_Bar_Background          : constant Palette_Index := Yellow;
   Default_Bar_Foreground          : constant Palette_Index := Light_Black;
   Default_Dialog_Background       : constant Palette_Index := White;
   Default_Dialog_Light_Foreground : constant Palette_Index := Light_White;
   Default_Dialog_Dark_Background  : constant Palette_Index := Black;

   -- For compatibility with terminals that do not support RBGA colours it is
   -- adviced to use the following convention for colour palettes. It is made
   -- for 8-colour terminals (such a TTY). So everything looks fine. It doesn't
   -- mean that you have to follow this convention you can choose the one you
   -- like.

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Palette_Type);

   function Nearest (
      Palette : in Palette_Type;
      Item    : in RGBA_Type)
      return Palette_Index;

   Default_Palette : constant Palette_Type := (
      0  =>  (31,  41,  59, 255),
      1  => (192,  48,  96, 255),
      2  =>  (48, 198,  96, 255),
      3  => (192, 192,  48, 255),
      4  =>  (48,  96, 192, 255),
      5  => (192,  48, 192, 255),
      6  =>  (48, 192, 192, 255),
      7  => (192, 192, 192, 255),
      8  =>  (62,  82, 118, 255),
      9  => (255,  64, 128, 255),
      10 =>  (64, 255, 128, 255),
      11 => (255, 255,  64, 255),
      12 =>  (64, 128, 255, 255),
      13 => (255,  64, 255, 255),
      14 =>  (64, 255, 255, 255),
      15 => (255, 255, 255, 255));

end Malef.Palettes;
