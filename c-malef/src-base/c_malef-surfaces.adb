-------------------------------------------------------------------------------
--                                                                           --
--                  C _ M A L E F - S U R F A C E S . A D B                  --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
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

with C_Malef.Errors;
with Malef.Exceptions;

package body C_Malef.Surfaces is
   use type Malef.Surfaces.Surface_Type;

   overriding
   function "=" (Left, Right : Surface_Type)
      return bool is
   begin

      return (if Left.Object = Right.Object then true else false);

   end "=";


   function Assign (Surface    : out Surface_Type;
      To_Surface : in  Surface_Type)
      return Error_Kind is
   begin

      Surface.Object := To_Surface.Object;

      return No_Error;

   exception
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Assign;


   function Create (
      Rows    : Row_Type;
      Cols    : Col_Type;
      Surface : in out Surface_Type)
      return Error_Kind is
   begin

      -- This might raise a very nasty exception if the surface isn't either
      -- null or other surface.
      Surface.Object := Malef.Surfaces.Create(Rows => Malef.Row_Type(Rows),
                                              Cols => Malef.Col_Type(Cols));

      return No_Error;

   exception
      when Ada_Exception : Constraint_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Bounds_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Create;


   function Clear (Surface : in out Surface_Type)
      return Error_Kind is
   begin

      Surface.Object.Clear;

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Null_Surface_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Null_Surface_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Clear;


   function Copy (Surface    : in Surface_Type;
      To_Surface : out Surface_Type)
      return Error_Kind is
   begin

      To_Surface.Object := Surface.Object.Copy;

      return No_Error;

   exception
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Copy;


   function Destroy (Surface : in out Surface_Type)
      return Error_Kind is
   begin

      -- This is enough to destroy a surface, controlled types and finalization
      -- function will take care of the rest.
      Surface := Get_Null_Surface;

      return No_Error;

   exception
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Destroy;


   function Get_Null_Surface return Surface_Type is
      Object : constant Malef.Surfaces.Surface_Type
             := Malef.Surfaces.Null_Surface;
   begin

      -- Due to the Object been a controlled type, trying to finalize the last
      -- Shared_Surface_Type it was pointing to will yield to a very nasty
      -- error, thus we must assing it without raising the attention of the
      -- Controlled types.
      return Surface_Type'(Object => Object);

   end Get_Null_Surface;


   function Resize (Surface : in out Surface_Type;
      Height : in Row_Type;
      Width  : in Col_Type)
      return Error_Kind is
   begin

      Surface.Object.Resize (
         Height => Malef.Row_Type(Height),
         Width  => Malef.Col_Type(Width));

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Null_Surface_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Null_Surface_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Resize;

end C_Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
