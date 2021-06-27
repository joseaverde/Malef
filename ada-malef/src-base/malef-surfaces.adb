-------------------------------------------------------------------------------
--                                                                           --
--                    M A L E F - S U R F A C E S . A D B                    --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
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

with Malef.Exceptions;


package body Malef.Surfaces is

   procedure Move_Cursor_To (Surface : Shared_Surface_Access;
                             Last    : Cursor_Type) with Inline is
   begin

      if Last.Col = Surface.Width then
         if Last.Row = Surface.Height then
            Surface.Cursor_Position := (1, 1);
         else
            Surface.Cursor_Position := (Last.Row + 1, 1);
         end if;
      else
         Surface.Cursor_Position := (Last.Row, Last.Col + 1);
      end if;

   end Move_Cursor_To;

   -------------------------------------------------------------------------

   function Create (
      Rows : Row_Type;
      Cols : Col_Type)
      return Surface_Type
   is
      New_Surface : Surface_Type;
   begin

      New_Surface.Reference := new Shared_Surface_Type;
      New_Surface.Reference.Height := Rows;
      New_Surface.Reference.Width  := Cols;
      New_Surface.Reference.Grid := new Matrix_Type (1 .. Rows, 1 .. Cols);
      New_Surface.Reference.Grid.all := (others => (others => Element_Type'(
         Format => Default_Format,
         Char   => Character'Pos(' '))));

      return New_Surface;

   end Create;


   function Get (Surface : Surface_Type;
      Position : Cursor_Type;
      Length   : Positive)
      return Str_Type
   is
      Reference : constant Shared_Surface_Access := Surface.Reference;
      Last_Col  : constant Col_Type := Position.Col + Col_Type(Length - 1);
      Str       : Str_Type (Positive(Position.Row) .. Natural(Last_Col));
   begin

      if Position.Row > Reference.Height or
         Last_Col     > Reference.Width
      then
         raise Malef.Exceptions.Bounds_Error
         with "String out of range!";
      end if;

      for Col in Str'Range loop
         Str (Col) := Reference.Grid (Position.Row, Col_Type(Col)).Char;
      end loop;

      Move_Cursor_To (Reference, (Position.Row, Last_Col));

      return Str;

   end Get;


   procedure Get (Surface : Surface_Type;
      Position : Cursor_Type;
      Item     : out Str_Type;
      Length   : out Positive)
   is
      Reference : constant Shared_Surface_Access := Surface.Reference;
   begin

      if Position.Row > Reference.Height or
         Position.Col > Reference.Width
      then
         raise Malef.Exceptions.Bounds_Error
         with "Position out of bounds!";
      end if;

      Length := Integer'Min (
         Integer(Reference.Width - Position.Col), Item'Length);
      for Col in Col_Type range
         Position.Col .. Position.Col + Col_Type (Length)
      loop
         Item (Item'First + Integer(Col) - Integer(Position.Col)) :=
            Reference.Grid (Position.Row, Col).Char;
      end loop;

      Move_Cursor_To (Reference,
         (Position.Row, Position.Col + Col_Type(Length)));

   end Get;


   procedure Get (Surface : Surface_Type;
      Item   : out Str_Type;
      Length : out Positive) is
   begin

      Surface.Get (Surface.Reference.Cursor_Position, Item, Length);

   end Get;


   function Get (Surface : Surface_Type;
      Position : Cursor_Type)
      return Char_Type
   is
      Reference : constant Shared_Surface_Access := Surface.Reference;
   begin

      if Position.Row > Reference.Height or
         Position.Col > Reference.Width
      then
         raise Malef.Exceptions.Bounds_Error
         with "Character out of bounds!";
      end if;

      Move_Cursor_To (Reference, Position);

      return Reference.Grid (Position.Row, Position.Col).Char;

   end Get;


   procedure Put (Surface : Surface_Type;
      Item     : Str_Type;
      Position : Cursor_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Reference;
      Last_Col  : constant Col_Type := Position.Col + Item'Length - 1;
   begin

      Check_Not_Null_Surface (Surface);

      if Position.Row > Reference.Height or
         Position.Col > Reference.Width  or
         Last_Col     > Reference.Width
      then
         raise Malef.Exceptions.Bounds_Error
         with "String out of bounds!";
      end if;

      for Col in Col_Type range Position.Col .. Last_Col loop
         Reference.Grid (Position.Row, Col) := Element_Type'(
            Format => Reference.Cursor_Format,
            Char   => Item(Positive(Col_Type(Item'First) + Col - Position.Col))
         );
      end loop;

      Move_Cursor_To (Reference, (Position.Row, Last_Col));

   end Put;


   procedure Put (Surface : Surface_Type;
      Item : Str_Type) is
   begin

      Surface.Put (Item, Surface.Reference.Cursor_Position);

   end Put;


   procedure Put (Surface : Surface_Type;
      Item     : Char_Type;
      Position : Cursor_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Reference;
   begin

      Check_Not_Null_Surface (Surface);

      if Position.Row > Reference.Height or
         Position.Col > Reference.Width
      then
         raise Malef.Exceptions.Bounds_Error
         with "Position out of bounds!";
      end if;

      Reference.Grid (Position.Row, Position.Col) := Element_Type'(
         Char   => Item,
         Format => Reference.Cursor_Format
      );

      Move_Cursor_To (Reference, Position);

   end Put;


   procedure Put (Surface : Surface_Type;
      Item : Char_Type) is
   begin

      Surface.Put (Item, Surface.Reference.Cursor_Position);

   end Put;


end Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
