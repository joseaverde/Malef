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
-- There are several functions that have no documentation and which are similar
-- to another one but without the Position argument. Well, these functions use
-- the cursor position as the position parameter.
--
-- Keep in mind that in every Get and Put functions the cursor will moved one
-- space to the right of the end of the string, character or surface put or
-- got. Also, if this position ends up out of bounds, then the it's moved to
-- the next row. And if both the row and column end up out of bounds, then it's
-- moved to the position (1, 1).
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
   function Create (
      Rows : Row_Type;
      Cols : Col_Type)
      return Surface_Type;

   -- TODO: procedure Copy

   --
   -- This function returns the string in a given position and moves the cursor
   -- just after the string ends. If it ended in a border of the surface, then
   -- it's moved to the following line.
   --
   -- @param Object
   -- The surface.
   --
   -- @param Position
   -- The position inside the Surface where the string starts.
   --
   -- @param Length
   -- The length of the string.
   --
   -- @return
   -- Returns the string in the given position of a given length.
   --
   -- @exception Malef.Exceptions.Bounds_Error
   -- This exception is raised when the string ends up out of bounds.
   --
   function Get (Surface : Surface_Type;
      Position : Cursor_Type;
      Length   : Positive)
      return Str_Type;

   function Get (Surface : Surface_Type;
      Length : Positive)
      return Str_Type is
      (Surface.Get(Surface.Get_Cursor_Position, Length)) with Inline;

   --
   -- As in the other Get functions, this ones moves the cursor to just after
   -- the character that was retrieved.
   --
   -- @param Object
   -- The surface.
   --
   -- @param Position
   -- The postion inside the Surfacce where the string starts.
   --
   -- @param Item
   -- The string that will be retrieved. It's length is taken, however if the
   -- string was too large for the surface, only the string until the surface
   -- border will be retrieved.
   --
   -- @param Length
   -- The length of the string retrieved.
   --
   procedure Get (Surface : Surface_Type;
      Position : Cursor_Type;
      Item     : out Str_Type;
      Length   : out Positive);

   procedure Get (Surface : Surface_Type;
      Item   : out Str_Type;
      Length : out Positive) with Inline;


   --
   -- This function returns the character in certain position and moves the
   -- cursor just after the character in the surface.
   --
   -- @param Object
   -- The surface.
   --
   -- @param Position
   -- The position of the character.
   --
   -- @exception Malef.Exceptions.Bounds_Error
   -- This exception is raised if the position was out of bounds.
   --
   function Get (Surface : Surface_Type;
      Position : Cursor_Type)
      return Char_Type;

   function Get (Surface : Surface_Type)
      return Char_Type is
      (Surface.Get(Surface.Get_Cursor_Position)) with Inline;

   -- TODO: Commment it
   procedure Put (Surface : Surface_Type;
      Item     : Str_Type;
      Position : Cursor_Type);

   procedure Put (Surface : Surface_Type;
      Item : Str_Type)
      with Inline;

   --
   -- This procedure puts a character onto the surface.
   --
   -- @param Object
   -- The surface.
   --
   -- @param Item
   -- The character to put.
   --
   procedure Put (Surface : Surface_Type;
      Item     : Char_Type;
      Position : Cursor_Type);

   procedure Put (Surface : Surface_Type;
      Item : Char_Type) with Inline;

   overriding procedure Update (Surface : in out Surface_Type) is null;

   Null_Surface : constant Surface_Type;

--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
private --*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--
--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-

   Null_Surface : constant Surface_Type
                := Surface_Type'(Ada.Finalization.Controlled with
                                    Shared_Null_Surface'Access);

end Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
