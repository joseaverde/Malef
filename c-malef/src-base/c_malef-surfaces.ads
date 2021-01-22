-------------------------------------------------------------------------------
--                                                                           --
--                  C _ M A L E F - S U R F A C E S . A D S                  --
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

with Malef.Surfaces;

--
-- @summary
--
--
-- @description
--
package C_Malef.Surfaces is

   type Surface_Type is
      record
         Object : Malef.Surfaces.Surface_Type := Malef.Surfaces.Null_Surface;
      end record;
   pragma Convention (C, Surface_Type);

   function Create (Rows    : Row_Type;
                    Cols    : Col_Type;
                    Surface : in out Surface_Type)
                    return Error_Kind;
   pragma Export (C, Create, "malef_createSurface");

   function Destroy (Object : in out Surface_Type)
                     return Error_Kind;
   pragma Export (C, Destroy, "malef_destroySurface");

   procedure Debug_Put (Object : Surface_Type);
   pragma Export (C, Debug_Put, "_malef_debugPutSurface");

   function Get_Null_Surface return Surface_Type;
   pragma Export (C, Get_Null_Surface, "malef_getNullSurface");

end C_Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
