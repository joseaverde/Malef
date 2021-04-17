-------------------------------------------------------------------------------
--                                                                           --
--          M A L E F - S U B S Y S T E M S - T E X T _ I O . A D S          --
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

-- TODO
-- @summary
--
--
-- @description
--
private package Malef.Subsystems.Text_IO is

   Length : constant := 1024;

   protected type Std_Out is
      entry Write (Data : in Str_Type);
      entry Write (Data : in Char_Type);
      entry Write (Data : in String);
      entry Dump;
   private
      Buffer  : String (1 .. Length + 1);
      Current : Natural  := 0;
      Lock    : Boolean  := False;
   end Std_Out;

end Malef.Subsystems.Text_IO;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
