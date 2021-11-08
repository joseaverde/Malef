-------------------------------------------------------------------------------
--                                                                           --
--               T E S T _ M A L E F - S U R F A C E S . A D S               --
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

package Test_Malef.Surfaces is

   procedure Main;

   -- This function shouldn't break anything, it should alsways work, because
   -- bounds are checked by the compiler and runtime.
   function Create return String;

   -- This function is just to test creation time it will create a very big
   -- surface to check the elapsed time.
   FUNCTION CREATE_BIG RETURN STRING;

   -- With the following functions we try writing on the surface in and out
   -- of bounds. Nothing should be written if an exception is raised. We have
   -- to check that too.
   function Put_Char return String;
   function Put_String return String;
   function Put_Char_Exception return String;
   function Put_String_Exception return String;

   -- Now we check that the values are the expected ones.
   function Get_Char return String;
   function Get_String return String;
   function Get_Char_Exception return String;
   function Get_String_Exception return String;

   -- We try to copy the surface into a new surface and check that the copy
   -- is good.
   function Copy return String;

   -- Finally we compare both surfaces if they are equal.
   function Compare return String;



end Test_Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
