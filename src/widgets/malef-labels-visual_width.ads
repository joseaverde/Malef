-------------------------------------------------------------------------------
--                                                                           --
--         M A L E F - L A B E L S - V I S U A L _ W I D T H . A D S         --
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

private function Malef.Labels.Visual_Width (
   Item : in Glyph)
   return Glyph_Width;
-- This function returns the width of a given character. This function isn't
-- developed yet because it requires a long research. But the idea is that
-- there are single width characters:
--
--    konnichiwa
--    # # # # #
--
-- Double width characters:
--
--    こんにちは
--    # # # # #
--
-- And there are characters that don't advance the cursor.
--
--    A` demain   (Imagine that 'A' + '`' compose 'À')
--    #  # # #
--
-- This function returns how many cells does the cursor advance when one
-- of those characters is printed.

pragma Preelaborate (Malef.Labels.Visual_Width);
