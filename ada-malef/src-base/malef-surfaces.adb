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

with Ada.Text_IO;
with Malef.Exceptions;
with Malef.Systems; use Malef.Systems;
with Ada.Unchecked_Deallocation;


package body Malef.Surfaces is

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


   procedure Debug_Put (Object : Surface_Type) is
      Buffer : String (1 .. 1024);
      Last   : Natural := Buffer'First;

      Surface : constant Shared_Surface_Access := Object.Reference;

      procedure Pop with Inline is
      begin

         Ada.Text_IO.Put(Buffer(Buffer'First .. Last - 1));
         Last := Buffer'First;

      end Pop;

      procedure Push (S : String) with Inline is
      begin

         if S'Length = 0 then
            Pop;
            return;
         end if;

         if S'Length + Last > Buffer'Last then
            Pop;
         end if;

         Buffer(Last .. Last + S'Length - 1) := S;
         Last := Last + S'Length;

      end Push;

      procedure Move (R : Row_Coord; C : Col_Coord) is
         R_Str : constant String := R'Image;
         C_Str : constant String := C'Image;
      begin

         Push(ASCII.ESC & '[' & R_Str(R_Str'First + 1 .. R_Str'Last) & ';' &
                                C_Str(C_Str'First + 1 .. C_Str'Last) & 'H');

      end Move;

      Char : Char_Type;
      Can  : Boolean;
      Last_Format : Format_Type := Default_Format;
   begin

      Push(Malef.Systems.Get_Format(Default_Format));
      for Row in Surface.Grid'Range(1) loop
         Move(Surface.Position.Row + Row_Coord(Row), Surface.Position.Col);
         for Col in Surface.Grid'Range(2) loop
            Char := Surface.Grid(Row, Col).Char;
            if Char = 0 then
               Move(Surface.Position.Row + Row_Coord(Row),
                    Surface.Position.Col + Col_Coord(Col)+1);
            else
               if Surface.Grid(Row, Col).Format /= Last_Format then
                  Last_Format := Surface.Grid(Row, Col).Format;
                  Push(Malef.Systems.Get_Format(Last_Format));
               end if;
               Can := False;
               Push (Character'Val(Char) & "");
            -- for C of Char loop
            --    if Can = False then
            --       if C /= 0 then
            --          Can := True;
            --          Push(Character'Val(C) & "");
            --       end if;
            --    else
            --       Push(Character'Val(C) & "");
            --    end if;
            -- end loop;
            end if;
         end loop;
      end loop;
      Push(Malef.Systems.Get_Format(Default_Format));
      Pop;

   end Debug_Put;


   function Height (Object : Surface_Type) return Row_Type is
      (Object.Reference.Height);

   function Get_Main_Surface return Surface_Type is (Main_Surface);

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

   end Put;


   procedure Resize (Object   : in out Surface_Type;
                     New_Rows : Row_Type;
                     New_Cols : Col_Type) is
      procedure Free is new Ada.Unchecked_Deallocation (
         Matrix_Type, Matrix_Access);
      New_Grid : Matrix_Access;
      Old_Grid : constant Matrix_Access := Object.Reference.Grid;
      function Min (A, B : Integer) return Integer is
         (if A > B then B else A) with Pure_Function, Inline;
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

   end Resize;


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
