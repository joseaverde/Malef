-------------------------------------------------------------------------------
--                                                                           --
--                    M A L E F - S U R F A C E S . A D S                    --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2021 José Antonio Verde Jiménez  All Rights Reserved  --
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

private with Ada.Finalization;

--
-- @summary
-- This child package defines the Surface_Type and its primitive operations.
--
-- @description
-- The Surface_Type is a Controlled type which means it is able to clean itself
-- when a scope finishes.
--
package Malef.Surfaces is

   type Surface_Type is tagged private;


   -- TODO: Replace this function.
   --
   -- This function creates a surface of a given number of rows and columns.
   --
   -- @param Rows
   -- The number of rows the surface has.
   --
   -- @param Cols
   -- The number of columns the new surface will have.
   --
   -- @return
   -- The surface itself.
   --
   function Create (Rows : Row_Type;
                    Cols : Col_Type)
                    return Surface_Type;

   -- TODO: Replace this function
   --
   -- This procedure doesn't belong to the API.
   -- This procedure prints the object onto the screen.
   --
   -- @param Object
   -- The object to put.
   --
   procedure Debug_Put (Object : Surface_Type);


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


   Null_Surface : constant Surface_Type;

--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
private --*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--
--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-

   --
   -- This is the Surface_Type: a Controlled type containing a Shared_Surface
   -- access type.
   --
   -- @field Reference
   -- The surface the current controlled type is pointing at.
   --
   type Surface_Type is new Ada.Finalization.Controlled with
      record
         Reference :  not null Shared_Surface_Access
                   := Shared_Null_Surface'Access;
      end record;

   --
   -- @param Object
   -- The object to initialize.
   --
   overriding procedure Initialize (Object : in out Surface_Type);

   --
   -- @param Object
   -- The object to adjust.
   --
   overriding procedure Adjust     (Object : in out Surface_Type);

   --
   -- @param Object
   -- The object to finalize.
   --
   overriding procedure Finalize   (Object : in out Surface_Type);

   --
   -- This procedure adds to the reference counter.
   --
   -- @param Item
   -- A not null shared surface access to increase its counter.
   --
   procedure Reference   (Item : not null Shared_Surface_Access);

   --
   -- This procedure substracts from the reference counter, and if it reaches
   -- zero it is freed.
   --
   -- @param Item
   -- A not null shared suraface access to decrease its counter. It will freed
   -- if it reaches 0.
   --
   procedure Unreference (Item : not null Shared_Surface_Access);



   Null_Surface : constant Surface_Type
                := Surface_Type'(Ada.Finalization.Controlled with
                                    Shared_Null_Surface'Access);
                     
end Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
