-------------------------------------------------------------------------------
--                                                                           --
--              M A L E F - I M P L E M E N T A T I O N . A D S              --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2021 José Antonio Verde Jiménez  All Rights Reserved  --
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

private package Malef.Implementation with
   Pure => True
is

   type Cell_Meta is
      record
         Bg_Name  : aliased Palette_Index;
         Fg_Name  : aliased Palette_Index;
         Has_Name : Boolean;
         Updated  : Boolean;
      end record with
      Object_Size => 32;

   for Cell_Meta use
      record
         Bg_Name  at 0 range 0 .. 7;
         Fg_Name  at 1 range 0 .. 7;
         -- 0 .. 5 unused
         Has_Name at 3 range 6 .. 6;
         Updated  at 3 range 7 .. 7;
      end record;

   type Cell_Type is
      record
         Foreground : aliased RGBA_Type;          -- 32 bits
         Background : aliased RGBA_Type;          -- 32 bits
         Meta       : aliased Cell_Meta;          -- 32 bits
         Character  : aliased Glyph;              -- 16 bits
         Style      : aliased Style_Type;         -- 10 bits
      end record with
      Object_Size => 128;

   type Matrix_Type is
      array (Row_Type range <>, Col_Type range <>)
      of aliased Cell_Type;

   Default_Cell : constant Cell_Type
                := Cell_Type'(Foreground => (255, 255, 255,   0),
                              Background => (  0,   0,   0,   0),
                              Character  => ' ',
                              Meta       => Cell_Meta'(Fg_Name  => 0,
                                                       Bg_Name  => 0,
                                                       Has_Name => False,
                                                       Updated  => False),
                              Style      => (others => False));

end Malef.Implementation;
