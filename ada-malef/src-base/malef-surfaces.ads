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

   --
   -- This function returns the Main Surface.
   --
   function Get_Main_Surface return Surface_Type with Inline, Pure_Function;

   --
   -- This function returns the height of a surface.
   --
   -- @param Object
   -- The surface
   --
   function Height (Object : Surface_Type) return Row_Type with Inline;

   --
   -- This function puts a surface "onto" another one in the relative
   -- position they would be depending on their coords, e.g:
   --
   --    +------------+
   --    |            |
   --    |       +-------------+
   --    |       |####|        |
   --    +-------|----+        |
   --            |             |
   --            +-------------+
   --
   -- The coloured part is the part that will be put onto the surface.
   --
   -- @param Object
   -- The object we want to put.
   --
   -- @param Onto
   -- The surface onto we want to put it.
   --
   -- @exception Malef.Exceptions.Null_Surface_Error
   -- This exception is raised if you are trying to put a surface onto a null
   -- surface, which cannot be overwritten.
   --
   procedure Put (Object : Surface_Type;
                  Onto   : Surface_Type := Get_Main_Surface);

   --
   -- This procedure changes the size of a surface and all variables that
   -- reference it, i.e:
   --
   --    declare
   --       -- Imagine you declare two surfaces.
   --       Surface_1 : Surface_Type;
   --       Surface_2 : Surface_Type;
   --    begin
   --       -- And you assign both to the same value.
   --       Surface_1 := ...;
   --       Surface_2 := Surface_1;
   --       -- Both will be referencing the same surface.
   --       -- Now if you resize one of them.
   --       Surface_2.Resize (10, 10);
   --       -- Then both of them will be resized.
   --       pragma Assert (Surface_1.Height = Surface_2.Height and
   --                      Surface_1.Width  = Surface_2.Width);
   --    end;
   --
   -- @param Object
   -- The Surface to be resized. If it was the Main_Surface, then it won't be
   -- resized unless the new number of rows and the new number of columns
   -- matches the one of the terminal. It's done this way so, it can be
   -- automatically resized when the terminal is resized.
   --
   -- @param New_Rows
   -- The new number of rows.
   --
   -- @param New_Cols
   -- The new number of columns.
   --
   -- @exception Malef.Exceptions.Null_Surface_Error
   -- You cannot resize a null surface.
   --
   procedure Resize (Object   : in out Surface_Type;
                     New_Rows : Row_Type;
                     New_Cols : Col_Type);

   --
   -- This function prints the main surface onto the screen.
   --
   procedure Update_Screen;

   --
   -- This function returns the width of a surface.
   --
   -- @param Object
   -- The surface.
   --
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
