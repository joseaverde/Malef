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
with Malef.Systems;


package body Malef.Surfaces is

   function Create (Rows : Row_Type;
                    Cols : Col_Type)
                    return Surface_Type is
      New_Surface : Surface_Type;
   begin

      New_Surface.Reference := new Shared_Surface_Type (Rows, Cols);
      New_Surface.Reference.Grid := (others => (others => Element_Type'(
         Format => Default_Format,
         Char   => Character'Pos('A'))));

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


   procedure Put (Object : in Surface_Type) is
   begin

      Malef.Systems.Put (Object.Reference);

   end Put;

--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
--*--*- private -*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-


end Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
