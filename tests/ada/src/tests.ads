-------------------------------------------------------------------------------
--                                                                           --
--                             T E S T S . A D S                             --
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


package Tests is

   TAB : constant Character := Character'Val(16#09#);

   type Unit_Type is access function return String;
   type Proc_Type is access procedure;

   procedure Safe_Run (Unit : Unit_Type);

   procedure Start (Pkg : String := "<>");

   procedure Test (Unit     : Unit_Type;
                   Name     : String;
                   Expected : String  := "<>";
                   Time_It  : Boolean := True);

   procedure Wait (Text : String);

   procedure Wrap (Proc : Proc_Type;
                   Name : String);

   Avoid_Delays : Boolean := False;

end Tests;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
