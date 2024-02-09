-------------------------------------------------------------------------------
--                                                                           --
--         M A L E F - L A B E L S - V I S U A L _ W I D T H . A D B         --
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

-- TODO: Make it better

function Malef.Labels.Visual_Width (
   Item : in Glyph with Unreferenced)
   return Glyph_Width is
begin
   -- https://stackoverflow.com/questions/19899554/unicode-range-for-japanese
   -- https://stackoverflow.com/questions/1366068/whats-the-complete-range-\
   -- for-chinese-characters-in-unicode
   case Item is
      when Glyph'Val (16#3000#) .. Glyph'Val (16#303F#)  -- Japanese Punctuat.
         | Glyph'Val (16#3040#) .. Glyph'Val (16#309F#)  -- ひらがな
         | Glyph'Val (16#30A0#) .. Glyph'Val (16#30FF#)  -- カタカナ
      -- | Glyph'Val (16#FF00#) .. Glyph'Val (16#FFEF#)  -- Full and Half Width
         | Glyph'Val (16#4E00#) .. Glyph'Val (16#9FFF#)  -- CJK unified ideogr.
         | Glyph'Val (16#3400#) .. Glyph'Val (16#4DBF#)     -- CJK Extension A
         | Glyph'Val (16#20000#) .. Glyph'Val (16#2A6DF#)   -- CJK Extension B
         | Glyph'Val (16#2A700#) .. Glyph'Val (16#2B73F#)   -- CJK Extension C
         | Glyph'Val (16#2B740#) .. Glyph'Val (16#2B81F#)   -- CJK Extension D
         | Glyph'Val (16#2B820#) .. Glyph'Val (16#2CEAF#)   -- CJK Extension E
         | Glyph'Val (16#2CEB0#) .. Glyph'Val (16#2EBEF#)   -- CJK Extension F
         | Glyph'Val (16#30000#) .. Glyph'Val (16#3134F#)   -- CJK Extension G
         | Glyph'Val (16#31350#) .. Glyph'Val (16#323AF#)   -- CJK Extension H
         | Glyph'Val (16#F900#)  .. Glyph'Val (16#FAFF#)    -- CJK Compat.
         | Glyph'Val (16#2F800#) .. Glyph'Val (16#2FA1F#)   -- CJK Compat. Sup.
         => return 2;
      when others
         => return 1;
   end case;
end Malef.Labels.Visual_Width;
