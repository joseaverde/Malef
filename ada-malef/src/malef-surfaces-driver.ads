-------------------------------------------------------------------------------
--                                                                           --
--             M A L E F - S U R F A C E S - D R I V E R . A D S             --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
--                                                                           --
-------------------------------------------------------------------------------
--     Copyright (c) 2020 José Antonio Verde Jiménez All Rights Reserved     --
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

--
-- @summary
-- This package doesn't belong to the API, it's only used to return the
-- reference of a type to other parts of the package without letting the user
-- being able to modify it themselves.
--
-- @description
-- This package contains just one public function that doesn't belong to the
-- API but can't be access by the user because it doesn't have an interface in
-- the library. It just returns the Shared_Surface_Access the Surface_Type is
-- pointing to so it can be modified in other parts of the package.
--
package Malef.Surfaces.Driver is

   --
   -- This function returns the Shared_Surface_Access a Surface_Type is
   -- pointing to.
   --
   -- @param Object
   -- The object to retrieve the Shared_Surface_Access from.
   --
   -- @return
   -- The shared surface access.
   --
   function Get_Reference (Object : in Surface_Type)
                           return Shared_Surface_Access;

end Malef.Surfaces.Driver;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
