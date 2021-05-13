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

   --
   -- This is the surface type, it's a controlled type that can be used to
   -- store a matrix of letters with colours and styles and then print it into
   -- the screen.
   --
   type Surface_Type is new Base_Type with null record;


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

   -- XXX: Depreciated
   --
   -- This procedure doesn't belong to the API.
   -- This procedure prints the object onto the screen.
   --
   -- @param Object
   -- The object to put.
   --
   procedure Debug_Put (Object : Surface_Type);

   function Get_Main_Surface return Surface_Type with Inline, Pure_Function;

   function Height (Object : Surface_Type) return Row_Type with Inline;

   -- TODO: Document it
   procedure Put (Object : Surface_Type;
                  Onto   : Surface_Type := Get_Main_Surface);

   -- All references will be also resized.
   procedure Resize (Object   : in out Surface_Type;
                     New_Rows : Row_Type;
                     New_Cols : Col_Type);

   procedure Update_Screen;

   function Width  (Object : Surface_Type) return Col_Type with Inline;

   Null_Surface : constant Surface_Type;

--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
private --*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--
--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-

   Null_Surface : constant Surface_Type
                := Surface_Type'(Ada.Finalization.Controlled with
                                    Shared_Null_Surface'Access);

   Main_Surface : Surface_Type
                := Surface_Type'(Ada.Finalization.Controlled with
                                    Shared_Main_Surface'Access);

end Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
