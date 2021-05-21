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
-- This package contains functions to work with surfaces.
--
-- @description
-- This package contains the functions to work with Surfaces from C. There is
-- a new surface type with the C convention that wraps the Ada type. This
-- package is written so the C side can use the Controlled type functionalities
-- from Ada.
--
package C_Malef.Surfaces is

   --
   -- This C type wraps the Ada surface type. It's made so you can also use
   -- the Ada Controlled types from the C binding, but you have to use the
   -- functions in this package. Also, it must be first initialized, otherwise
   -- it will raise a Segmentation Fault error.
   --
   -- @field Object
   -- It's the Ada controlled type.
   --
   type Surface_Type is
      record
         Object : Malef.Surfaces.Surface_Type := Malef.Surfaces.Null_Surface;
      end record
   with Convention => C;


   --
   -- This function is used to assign an object to another and keep the
   -- reference count. Both must be initialized, otherwise it won't work.
   --
   -- @param Surface
   -- The surface we want to get assigned to another one.
   --
   -- @param To_Surface
   -- The surface it will be assigned to.
   --
   -- @return
   -- It returns Ada_Error if something went wrong.
   --
   function Assign (Surface    : out Surface_Type;
                    To_Surface : in  Surface_Type)
                    return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_assignSurface";

   --
   -- This function creates an empty surface of a given number of Rows (Height)
   -- and Columns (Width).
   --
   -- @param Rows
   -- The height, the number of rows.
   --
   -- @param Cols
   -- The width, the number of columns.
   --
   -- @param Surface
   -- The surface object that we want to modify to contain the new surface.
   --
   -- @return
   -- It returns if there was an error. If the number of rows or columns is not
   -- in range 1 .. Row/Col_Type'Last, then a Bounds_Error is returned,
   -- otherwise an Ada_Error is returned as always.
   --
   function Create (Rows    : Row_Type;
                    Cols    : Col_Type;
                    Surface : in out Surface_Type)
                    return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_createSurface";

   --
   -- This function unreferences a Surface, it doesn't actually destroy it, I
   -- just couldn't find a good name at the moment and this one sounds cool. A
   -- Surface can't be never deallocated unless all the variables that
   -- reference it have completely fade away. So no `free'-like function is
   -- available. This is the same as falling out of the scope in Ada.
   --
   -- @param Surface
   -- The surface to unreference.
   --
   -- @return
   -- It returns an Ada_Error if the Surface hasn't been initialised.
   --
   function Destroy (Surface : in out Surface_Type)
                     return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_destroySurface";

   --
   -- This function just returns a null surface and references it. It can't
   -- raise any error because there is no surface involved.
   --
   -- @return
   -- It returns a null surface for initialization.
   --
   function Get_Null_Surface return Surface_Type
      with Export        => True,
           Convention    => C,
           External_Name => "malef_getNullSurface";

end C_Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
