-------------------------------------------------------------------------------
--                                                                           --
--          M A L E F - S U B S Y S T E M S - T E X T _ I O . A D B          --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
--                                                                           --
-------------------------------------------------------------------------------
--     Copyright (c) 2021 José Antonio Verde Jiménez All Rights Reserved     --
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

with Malef.Characters;
with Interfaces.C_Streams;

package body Malef.Subsystems.Text_IO is

   protected body Std_Out is

      procedure Dump_It with Inline_Always;
      procedure Dump_It is
         Dummy : Integer
            with Unreferenced;
      begin

         Buffer (Current + 1) := Character'Val(0);
         Dummy := Interfaces.C_Streams.fputs (
            Strng => Buffer'Address,
            Stream => Interfaces.C_Streams.stdout
         );
         Current := 0;

      end Dump_It;

      procedure Write_Char (Char : String) with Inline_Always;
      procedure Write_Char (Char : String) is
      begin

         if Char'Length + Current > Length then
            Dump_It;
         end if;
         Buffer (Current + 1 .. Current + Char'Length) := Char;
         Current := Current + Char'Length;

      end Write_Char;


      entry Write (Data : in Str_Type)
         when not Lock is
      begin

         Lock := True;

         for Char of Data loop
            -- We convert the character to the current encoding system, for now
            -- it will be UTF-8.
            -- TODO: Add more encodings.
            Write_Char (Malef.Characters.To_UTF8 (Char));
         end loop;

         Lock := False;

      end Write;


      entry Write (Data : in Char_Type)
         when not Lock is
      begin

         Lock := True;
         Write_Char (Malef.Characters.To_UTF8 (Data));
         Lock := False;

      end Write;


      entry Write (Data : in String)
         when not Lock is

         First : Natural := Data'First;
         Last  : Natural := Data'Last;
      begin

         Lock := True;
         if Data'Length + Current > Length then
            Dump_It;
         end if;
         while Last - First > Length loop
            Buffer (Buffer'First .. Buffer'Last - 1) :=
               Data (First .. First + Length - 1);
            First := First + Length;
            Current := Length;
            Dump_It;
         end loop;
         Write_Char (Data (First .. Last));
         Lock := False;

      end Write;

      entry Dump
         when not Lock is
      begin

         Lock := True;
         Dump_It;
         Lock := False;

      end Dump;

   end Std_Out;

end Malef.Subsystems.Text_IO;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
