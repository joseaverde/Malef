-------------------------------------------------------------------------------
--                                                                           --
--                    M A L E F - S U R F A C E S . A D B                    --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
--                                                                           --
-------------------------------------------------------------------------------
--     Copyright (c) 2020 José Antonio Verde Jiménez All Rights Reserved     --
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
with Ada.Unchecked_Deallocation;
with Malef.Systems;
with System.Atomic_Counters;


package body Malef.Surfaces is

   function Create (Rows : Row_Type;
                    Cols : Col_Type)
                    return Surface_Type is
      New_Surface : Surface_Type;
   begin

      New_Surface.Reference := new Shared_Surface_Type (Rows, Cols);
      New_Surface.Reference.Grid := (others => (others => Element_Type'(
         Format => Default_Format,
         Char   => (0, 0, 0, Character'Pos('A')))));

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

      procedure Move (R : Row_Type; C : Col_Type) is
         R_Str : constant String := R'Image;
         C_Str : constant String := C'Image;
      begin

         Push(ASCII.ESC & '[' & R_Str(R_Str'First + 1 .. R_Str'Last) & ';' &
                                C_Str(C_Str'FIrst + 1 .. C_Str'Last) & 'H');

      end Move;

      Char : Char_Type;
      Can  : Boolean;
      Last_Format : Format_Type := Default_Format;
   begin

      Push(Malef.Systems.Get_Format(Default_Format));
      for Row in Surface.Grid'Range(1) loop
         Move(Surface.Position.Row + Row, Surface.Position.Col);
         for Col in Surface.Grid'Range(2) loop
            Char := Surface.Grid(Row, Col).Char;
            if Char = (0, 0, 0, 0) then
               Move(Surface.Position.Row + Row, Surface.Position.Col+Col+1);
            else
               if Surface.Grid(Row, Col).Format /= Last_Format then
                  Last_Format := Surface.Grid(Row, Col).Format;
                  Push(Malef.Systems.Get_Format(Last_Format));
               end if;
               Can := False;
               for C of Char loop
                  if Can = False then
                     if C /= 0 then
                        Can := True;
                        Push(Character'Val(C) & "");
                     end if;
                  else
                     Push(Character'Val(C) & "");
                  end if;
               end loop;
            end if;
         end loop;
      end loop;
      Push(Malef.Systems.Get_Format(Default_Format));
      Pop;

   end Debug_Put;

            
   function Get_Reference (Object : in Surface_Type)
                           return Shared_Surface_Access is
   begin

      return Object.Reference;

   end Get_Reference;


--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
--*--*- private -*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-


   overriding
   procedure Initialize (Object : in out Surface_Type) is
   begin

      Reference (Object.Reference);

   end Initialize;


   overriding
   procedure Adjust (Object : in out Surface_Type) is
   begin

      Reference (Object.Reference);

   end Adjust;

   
   overriding
   procedure Finalize (Object : in out Surface_Type) is
      Old_Reference :  constant not null Shared_Surface_Access
                    := Object.Reference;
   begin

      if Old_Reference /= Shared_Null_Surface'Access then
         -- This is used to avoid finalizing the same object twice.
         Object.Reference := Shared_Null_Surface'Access;
         Unreference(Old_Reference);
      end if;

   end Finalize;



   procedure Reference (Item : not null Shared_Surface_Access) is
   begin

      if Item = Shared_Null_Surface'Access then
         return;
      end if;

      System.Atomic_Counters.Increment (Item.Counter);

   end Reference;


   procedure Unreference (Item : not null Shared_Surface_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Shared_Surface_Type,
                                                        Shared_Surface_Access);
      Old : Shared_Surface_Access := Item;
   begin

      if Old = Shared_Null_Surface'Access then
         return;
      end if;

      if System.Atomic_Counters.Decrement (Old.Counter) then
         Free(Old);
      end if;

   end Unreference;

end Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
