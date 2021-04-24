-------------------------------------------------------------------------------
--                                                                           --
--                  M A L E F - C H A R A C T E R S . A D S                  --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
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

-- TODO: Add description.
-- @summary
--
--
-- @description
--
package Malef.Characters is

   -- Non printable characters.
   SKIP         : constant Char_Type := 0;
   TRANSPARENT  : constant Char_Type := 1;
   DOUBLE_WIDTH : constant Char_Type := 2;

   function To_UTF8 (Char : Char_Type)
                     return String
      with Inline;

   function To_UTF8 (Str : Str_Type)
                     return String
      with Inline;

   function From_UTF8 (Str : String)
                       return Str_Type
      with Pre    => (Char_Type(Character'Pos(Str(Str'First))) and
                      2#10_000000#) /= 0,
           Inline;


end Malef.Characters;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
