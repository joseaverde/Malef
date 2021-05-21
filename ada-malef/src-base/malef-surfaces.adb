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
with Malef.Systems; use Malef.Systems;
with Ada.Unchecked_Deallocation;


package body Malef.Surfaces is

   function Min (A, B : Integer) return Integer is
      (if A < B then A else B) with Pure_Function, Inline_Always;

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

   procedure Check_Not_Null_Surface (Surface : Surface_Type) with Inline is
   begin
      if Surface.Reference = Shared_Null_Surface'Access then
         raise Malef.Exceptions.Null_Surface_Error
         with "Cannot modify a null surface!";
      end if;
   end Check_Not_Null_Surface;


   function Create (Rows : Row_Type;
                    Cols : Col_Type)
                    return Surface_Type is
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


   function Cursor_Position (Object : Surface_Type) return Cursor_Type is
      (Object.Reference.Cursor_Position);


   function Get (Object   : Surface_Type;
                 Position : Cursor_Type;
                 Length   : Positive)
                 return Str_Type is
      Surface  : constant Shared_Surface_Access := Object.Reference;
      Last_Col : constant Col_Type := Position.Col + Col_Type(Length - 1);
      Str      : Str_Type (Positive(Position.Row) .. Natural(Last_Col));
   begin

      if Position.Row > Surface.Height or
         Last_Col     > Surface.Width
      then
         raise Malef.Exceptions.Bounds_Error
         with "String out of range!";
      end if;

      for Col in Str'Range loop
         Str (Col) := Surface.Grid (Position.Row, Col_Type(Col)).Char;
      end loop;

      Move_Cursor_To (Surface, (Position.Row, Last_Col));

      return Str;

   end Get;


   procedure Get (Object   : Surface_Type;
                  Position : Cursor_Type;
                  Item     : out Str_Type;
                  Length   : out Positive) is
      Surface : constant Shared_Surface_Access := Object.Reference;
   begin

      if Position.Row > Surface.Height or
         Position.Col > Surface.Width
      then
         raise Malef.Exceptions.Bounds_Error
         with "Position out of bounds!";
      end if;

      Length := Min (Integer(Surface.Width - Position.Col), Item'Length);
      for Col in Col_Type range
         Position.Col .. Position.Col + Col_Type (Length)
      loop
         Item (Item'First + Integer(Col) - Integer(Position.Col)) :=
            Surface.Grid (Position.Row, Col).Char;
      end loop;

      Move_Cursor_To (Surface, (Position.Row, Position.Col + Col_Type(Length)));

   end Get;


   procedure Get (Object : Surface_Type;
                  Item   : out Str_Type;
                  Length : out Positive) is
   begin

      Object.Get (Object.Reference.Cursor_Position, Item, Length);

   end Get;


   function Get (Object   : Surface_Type;
                 Position : Cursor_Type)
                 return Char_Type is
      Surface : constant Shared_Surface_Access := Object.Reference;
   begin

      if Position.Row > Surface.Height or
         Position.Col > Surface.Width
      then
         raise Malef.Exceptions.Bounds_Error
         with "Character out of bounds!";
      end if;

      Move_Cursor_To (Surface, Position);

      return Surface.Grid (Position.Row, Position.Col).Char;

   end Get;


   function Get_Main_Surface return Surface_Type is (Main_Surface);


   function Height (Object : Surface_Type) return Row_Type is
      (Object.Reference.Height);

   function Position (Object : Surface_Type) return Coord_Type is
      (Object.Reference.Position);


   procedure Put (Object : Surface_Type;
                  Onto   : Surface_Type := Get_Main_Surface) is
      In_Surface  : constant Shared_Surface_Access := Object.Reference;
      Out_Surface : constant Shared_Surface_Access := Onto.Reference;

      --
      -- We need to find two ranges, one for the X axis (cols) and another one
      -- for the Y axis (rows). It depends on the relative position of both
      -- surfaces.
      --
      -- Let's start with the X axis (cols)
      From_Col : constant Col_Type := (
         -- We check if the surface "onto" we want to print our surface is on
         -- the left side (True) or on the right side (False). If they are in
         -- the same position (True), then the first row in the range will be
         -- the first one.
         if In_Surface.Position.Col >= Out_Surface.Position.Col
            --
            -- Out_Surface (2)  In_Surface (1)
            -- +-------+-----+---------------+
            -- |       |#####|               |
            -- |       |#####|               |
            -- +-------+-----+---------------+
            --
            -- x1 > x2 -> We start printing from the begining of In_Surface.
            -- x2 > x1 -> We count the offset
            --
         then In_Surface.Grid'First(2)       -- 1
         else Col_Type(1 + Out_Surface.Position.Col - In_Surface.Position.Col)
      );
      -- We have to use integer, so no uncatchable Constraint_Error exception
      -- is raised if the "out" surface is too far to the right.
      To_Col : constant Integer := Integer(
         -- It's the case oposite to the one above.
         if In_Surface.Position.Col <= Out_Surface.Position.Col
         then In_Surface.Grid'Last(2)        -- 1
         else Col_Type(In_Surface.Position.Col + In_Surface.Grid'Length(2) -
               Out_Surface.Position.Col)
      );

      -- For the rows is the same as above.
      From_Row : constant Row_Type := (
         if In_Surface.Position.Row >= Out_Surface.Position.Row
         then In_Surface.Grid'First(1)
         else Row_Type(1 +  Out_Surface.Position.Row - In_Surface.Position.Row)
      );
      To_Row : constant Integer := Integer(
         if In_Surface.Position.Row <= Out_Surface.Position.Row
         then In_Surface.Grid'Last(1)
         else Row_Type(In_Surface.Position.Row + In_Surface.Grid'Length(1) -
               Out_Surface.Position.Row)
      );

      --
      -- Finally we have to check the starting position for the destination
      -- (out) surface.
      --
      First_Col : constant Col_Type := (
         -- It's the oposite case of From_Col.
         if Out_Surface.Position.Col >= In_Surface.Position.Col
         then Out_Surface.Grid'First(2)    -- 1
         else Col_Type(1 + In_Surface.Position.Col - Out_Surface.Position.Col)
      );
      First_Row : constant Row_Type := (
         if Out_Surface.Position.Row >= In_Surface.Position.Row
         then Out_Surface.Grid'First(1)
         else Row_Type(1 + In_Surface.Position.Row - Out_Surface.Position.Row)
      );

      The_Row : Row_Type;
      The_Col : Col_Type;

   begin

      Check_Not_Null_Surface (Onto);

      -- We check that the ranges are ok.
      if From_Col > In_Surface.Grid'Last(2) or
         To_Col < Integer(From_Col)         or
         From_Row > In_Surface.Grid'Last(1) or
         To_Row < Integer(From_Row)
      then
         return;
      end if;

      for Row in Row_Type range From_Row .. Row_Type(To_Row) loop
         for Col in Col_Type range From_Col .. Col_Type(To_Col) loop
            The_Row := First_Row + Row - From_Row;
            The_Col := First_Col + Col - From_Col;
         -- exit when The_Row not in Out_Surface.Grid'Range(1) or
         --           The_Col not in Out_Surface.Grid'Range(2);
            Out_Surface.Grid (The_Row, The_Col) := In_Surface.Grid (Row, Col);
         end loop;
      end loop;

      Move_Cursor_To (Out_Surface, (Row_Type(To_Row), Col_Type(To_Col)));

   end Put;


   procedure Put (Object   : Surface_Type;
                  Onto     : Surface_Type := Get_Main_Surface;
                  Position : Cursor_Type) is
      In_Surface  : constant Shared_Surface_Access := Object.Reference;
      Out_Surface : constant Shared_Surface_Access := Onto.Reference;

      Last_Row : constant Row_Type := Row_Type (Min (
         Integer(Out_Surface.Height),
         Integer(In_Surface.Height + Position.Row)));
      Last_Col : constant Col_Type := Col_Type (Min (
         Integer(Out_Surface.Width),
         Integer(In_Surface.Width + Position.Col)));
   begin

      Check_Not_Null_Surface (Onto);

      if Position.Row > Out_Surface.Height or
         Position.Col > Out_Surface.Width
      then
         raise Malef.Exceptions.Bounds_Error
         with "Position out of surfaces bounds!";
      end if;

      for Row in Row_Type range Position.Row .. Last_Row loop
         for Col in Col_Type range Position.Col .. Last_Col loop
            Out_Surface.Grid (Row, Col) := In_Surface.Grid (
               1 + Row - Position.Row,
               1 + Col - Position.Col);
         end loop;
      end loop;

      Move_Cursor_To (Out_Surface, (Last_Row, Last_Col));

   end Put;


   procedure Put (Object   : Surface_Type;
                  Item     : Str_Type;
                  Position : Cursor_Type) is
      Surface  : constant Shared_Surface_Access := Object.Reference;
      Last_Col : constant Col_Type := Position.Col + Item'Length - 1;
   begin

      Check_Not_Null_Surface (Object);

      if Position.Row > Surface.Height or
         Position.Col > Surface.Width  or
         Last_Col     > Surface.Width
      then
         raise Malef.Exceptions.Bounds_Error
         with "String out of bounds!";
      end if;

      for Col in Col_Type range Position.Col .. Last_Col loop
         Surface.Grid (Position.Row, Col) := Element_Type'(
            Format => Surface.Cursor_Format,
            Char   => Item(Positive(Col_Type(Item'First) + Col - Position.Col))
         );
      end loop;

      Move_Cursor_To (Surface, (Position.Row, Last_Col));

   end Put;


   procedure Put (Object : Surface_Type;
                  Item   : Str_Type) is
   begin

      Object.Put (Item, Object.Reference.Cursor_Position);

   end Put;


   procedure Put (Object   : Surface_Type;
                  Item     : Char_Type;
                  Position : Cursor_Type) is
      Surface : constant Shared_Surface_Access := Object.Reference;
   begin

      Check_Not_Null_Surface (Object);

      if Position.Row > Surface.Height or
         Position.Col > Surface.Width
      then
         raise Malef.Exceptions.Bounds_Error
         with "Position out of bounds!";
      end if;

      Surface.Grid (Position.Row, Position.Col) := Element_Type'(
         Char   => Item,
         Format => Surface.Cursor_Format
      );

      Move_Cursor_To (Surface, Position);

   end Put;


   procedure Put (Object : Surface_Type;
                  Item   : Char_Type) is
   begin

      Object.Put (Item, Object.Reference.Cursor_Position);

   end Put;



   procedure Resize (Object   : in out Surface_Type;
                     New_Rows : Row_Type;
                     New_Cols : Col_Type) is
      procedure Free is new Ada.Unchecked_Deallocation (
         Matrix_Type, Matrix_Access);
      New_Grid : Matrix_Access;
      Old_Grid : constant Matrix_Access := Object.Reference.Grid;
   begin

      Check_Not_Null_Surface (Object);

      if Object.Reference = Shared_Main_Surface'Access and then
         (New_Rows /= Malef.Height or New_Cols /= Malef.Width)
      then
         -- Cannot resize the main surface unless it's to the size of the
         -- current window.
         return;
      end if;

      if New_Rows = Object.Reference.Height and
         New_Cols = Object.Reference.Width
      then
         -- Nothing to be done.
         return;
      end if;

      New_Grid := new Matrix_Type (1 .. New_Rows, 1 .. New_Cols);
      New_Grid.all := (others => (others => Default_Element));
      for Row in Row_Type range 1 .. Row_Type
            (Min(Integer(New_Rows), Integer(Object.Reference.Height)))
         loop
         for Col in Col_Type range 1 .. Col_Type
            (Min (Integer(New_Cols), Integer(Object.Reference.Width)))
         loop
            New_Grid (Row, Col) := Old_Grid (Row, Col);
         end loop;
      end loop;

      Free (Object.Reference.Grid);
      Object.Reference.Grid := New_Grid;
      Object.Reference.Height := New_Rows;
      Object.Reference.Width  := New_Cols;
      Object.Reference.Cursor_Position := (1, 1);

   end Resize;


   procedure Set_Cursor_Position (Object   : Surface_Type;
                                  Position : Cursor_Type) is
      Surface : constant Shared_Surface_Access := Object.Reference;
   begin

      if Position.Row > Surface.Height or
         Position.Col > Surface.Width
      then
         raise Malef.Exceptions.Bounds_Error
         with "Cursor Position out of bounds!";
      end if;

      Object.Reference.Cursor_Position := Position;

   end Set_Cursor_Position;


   procedure Update_Screen is
   begin

      Loaded_Subsystems(Current_Subsystem).Put;

   end Update_Screen;


   function Width  (Object : Surface_Type) return Col_Type is
      (Object.Reference.Width);

--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
--*--*- private -*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-


end Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
