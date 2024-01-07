-------------------------------------------------------------------------------
--                                                                           --
--                    M A L E F - P A L E T T E S . A D S                    --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
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

   type Palette_Type is array (Palette_Index) of RGBA_Type;

   Default_Palette : constant Palette_Type := (
      0  => ( 31,  41,  59, 255),
      1  => (192,  48,  96, 255),
      2  => ( 48, 198,  96, 255),
      3  => (192, 192,  48, 255),
      4  => ( 48,  96, 192, 255),
      5  => (192,  48, 192, 255),
      6  => ( 48, 192, 192, 255),
      7  => (192, 192, 192, 255),
      8  => ( 62,  82, 118, 255),
      9  => (255,  64, 128, 255),
      10 => ( 64, 255, 128, 255),
      11 => (255, 255,  64, 255),
      12 => ( 64, 128, 255, 255),
      13 => (255,  64, 255, 255),
      14 => ( 64, 255, 255, 255),
      15 => (255, 255, 255, 255));

   function Nearest (
      Palette : in Palette_Type;
      Item    : in RGBA_Type)
      return Palette_Index;

end Malef.Palettes;
