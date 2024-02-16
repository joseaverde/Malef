-------------------------------------------------------------------------------
--                                                                           --
--    M A L E F - L A B E L S - P L A I N _ T E X T _ P A R S E R . A D B    --
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

with Ada.Wide_Wide_Characters.Handling;
with Malef.Labels.Visual_Width;

function Malef.Labels.Plain_Text_Parser (
   Item : in Unbounded_Wide_Wide_String)
   return Markup_Text
is
   use Ada.Wide_Wide_Characters.Handling;
   Copy : constant Rich_Glyph := (
      Value      => Nul,
      Kind       => Normal,
      Width      => 1,
      Style      => [others => False],
      Background => [0, 0, 0, 0],
      Foreground => [0, 0, 0, 0]);
   Char : Glyph;
begin
   return Result : Markup_Text do
      for I in 1 .. Length (Item) loop
         Char := Element (Item, I);
         Result.Text.Append ((Copy with delta
                              Value => Char,
                              Kind  => (if Is_Line_Terminator (Char)
                                          then Paragraph_Break else Normal),
                              Width => Visual_Width (Char)));
      end loop;
   end return;
end Malef.Labels.Plain_Text_Parser;
