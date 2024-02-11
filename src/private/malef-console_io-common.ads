-------------------------------------------------------------------------------
--                                                                           --
--           M A L E F - C O N S O L E _ I O - C O M M O N . A D S           --
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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Text_IO;

private package Malef.Console_IO.Common is

   package Unicode renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

   procedure Get_Immediate (
      Item : out Character)
      renames Ada.Text_IO.Get_Immediate;

   procedure Get_Immediate (
      Item      : out Character;
      Available : out Boolean)
      renames Ada.Text_IO.Get_Immediate;

   procedure Get_Immediate (
      Item      : out Wide_Wide_Character;
      Available : out Boolean);
      -- renames Ada.Wide_Wide_Text_IO.Get_Immediate;

end Malef.Console_IO.Common;
