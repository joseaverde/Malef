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
   function Create (Rows : Row_Type;
                    Cols : Col_Type)
                    return Surface_Type;

   --
   -- This function returns the cursor position of an surface
   --
   -- @param Object
   -- The surface itself.
   --
   -- @return
   -- The position of the cursor inside a a surface.
   --
   function Cursor_Position (Object : Surface_Type) return Cursor_Type;


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
   function Get (Object   : Surface_Type;
                 Position : Cursor_Type;
                 Length   : Positive)
                 return Str_Type;

   function Get (Object : Surface_Type;
                 Length : Positive)
                 return Str_Type is
      (Object.Get(Object.Cursor_Position, Length)) with Inline;

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
   procedure Get (Object   : Surface_Type;
                  Position : Cursor_Type;
                  Item     : out Str_Type;
                  Length   : out Positive);

   procedure Get (Object : Surface_Type;
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
   function Get (Object   : Surface_Type;
                 Position : Cursor_Type)
                 return Char_Type;

   function Get (Object : Surface_Type)
                 return Char_Type is
      (Object.Get(Object.Cursor_Position)) with Inline;

   --
   -- This function returns the Main Surface.
   --
   function Get_Main_Surface return Surface_Type with Inline, Pure_Function;

   --
   -- This function returns the height of a surface.
   --
   -- @param Object
   -- The surface.
   --
   -- @return
   -- The height.
   --
   function Height (Object : Surface_Type) return Row_Type with Inline;

   --
   -- This function returns the position of the surface in the screen.
   --
   -- @param Object
   -- The surface.
   --
   -- @return
   -- The position of the object in the screen.
   --
   function Position (Object : Surface_Type) return Coord_Type with Inline;


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
   -- This function puts a surface "onto" another one given the position inside
   -- the output surface.
   --
   -- @param Object
   -- The object we want to put.
   --
   -- @param Onto
   -- The surface we want to put it onto.
   --
   -- @exception Malef.Exceptions.Bounds_Error.
   -- This exception is raised when the position of the surface is out of
   -- bounds.
   procedure Put (Object   : Surface_Type;
                  Onto     : Surface_Type := Get_Main_Surface;
                  Position : Cursor_Type);

   procedure Put (Object   : Surface_Type;
                  Item     : Str_Type;
                  Position : Cursor_Type
                  );--TODO: Mode     : Surface_Mode := Normal);

   procedure Put (Object   : Surface_Type;
                  Item     : Str_Type
                  );--TODO Mode     : Surface_Mode := Normal) with Inline;

   --
   -- This procedure puts a character onto the surface.
   --
   -- @param Object
   -- The surface.
   --
   -- @param Item
   -- The character to put.
   --
   procedure Put (Object   : Surface_Type;
                  Item     : Char_Type;
                  Position : Cursor_Type);

   procedure Put (Object   : Surface_Type;
                  Item     : Char_Type) with Inline;

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
   -- This function changes the cursor position.
   --
   -- @param Object
   -- The surface.
   --
   -- @param Position
   -- The new position of the cursor.
   --
   -- @exception Malef.Exceptions.Bounds_Error
   -- This exception is raised if the cursor is out of the surface's bounds.
   procedure Set_Cursor_Position (Object   : Surface_Type;
                                  Position : Cursor_Type) with Inline;

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
