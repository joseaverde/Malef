-------------------------------------------------------------------------------
--                                                                           --
--              M A L E F - P L A T F O R M - T E R M I N A L -              --
--                    G E N E R I C _ B U F F E R . A D B                    --
--                                                                           --
--                                 M A L E F                                 --
--                                  A N S I                                  --
--                                                                           --
--                              A D A   B O D Y                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2021-2024 José Antonio Verde Jiménez  All Rights Reserved  --
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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Malef.Platform.Generic_Buffer is

   package Unicode renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

   Index : Natural := 0;
   Data  : String (1 .. Capacity);

   procedure Flush is
   begin
      String'Write (Stream, Data (1 .. Index));
      Index := 0;
   end Flush;

   procedure Put (Item : in Character) is
   begin
      if Index = Capacity then
         Flush;
      end if;
      Index := @ + 1;
      Data (Index) := Item;
   end Put;

   procedure Put (Item : in String) is
   begin
      if Index + Item'Length > Capacity then
         Flush;
      end if;
      Data (Index + 1 .. Index + Item'Length) := Item;
      Index := @ + Item'Length;
   end Put;

   procedure Wide_Wide_Put (Item : in Glyph) is
   begin
      -- OPTIMISE: Search a function on character basis instead of strings.
      case Item is
         when   Nul  => Put (' ');
         when   Dbl  => null;
         when   Bck  => Put ("  ");
         when others => Put (Unicode.Encode (Item & ""));
      end case;
   end Wide_Wide_Put;

   procedure Wide_Wide_Put (Item : in Glyph_String) is
   begin
      for Char of Item loop
         Wide_Wide_Put (Char);
      end loop;
   end Wide_Wide_Put;

end Malef.Platform.Generic_Buffer;
