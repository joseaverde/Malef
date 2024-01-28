-------------------------------------------------------------------------------
--                                                                           --
--                  M A L E F - C O N S O L E _ I O . A D S                  --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   S P E C                              --
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

with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Malef.Console_IO is

   package Unicode renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   package T_IO renames Ada.Text_IO;
   package T_IO_Streams renames T_IO.Text_Streams;

   Stream : constant T_IO_Streams.Stream_Access
          := T_IO_Streams.Stream (T_IO.Standard_Output);
   Max_Capacity  : constant := 128;

   type Buffer_Type (
      Capacity : Positive) is
      record
         Index : Natural := 0;
         Data  : String (1 .. Capacity);
      end record;

   procedure Flush (
      Buffer : in out Buffer_Type) is
   begin
      String'Write (Stream, Buffer.Data (1 .. Buffer.Index));
      Buffer.Index := 0;
   end Flush;

   procedure Put (
      Buffer : in out Buffer_Type;
      Item   : in     Character) is
   begin
      if Buffer.Index = Buffer.Capacity then
         Flush (Buffer);
      end if;
      Buffer.Index := @ + 1;
      Buffer.Data (Buffer.Index) := Item;
   end Put;

   procedure Put (
      Buffer : in out Buffer_Type;
      Item   : in     String) is
   begin
      if Buffer.Index + Item'Length > Buffer.Capacity then
         Flush (Buffer);
      end if;
      Buffer.Data (Buffer.Index + 1 .. Buffer.Index + Item'Length) := Item;
      Buffer.Index := @ + Item'Length;
   end Put;

   procedure Wide_Wide_Put (
      Buffer : in out Buffer_Type;
      Item   : in     Glyph) is
   begin
      if Glyph'Pos (Item) < 32 then
         Put (Buffer, ' ');
      else
         -- OPTIMISE: Search a function on character basis instead of strings.
         Put (Buffer, Unicode.Encode (Item & ""));
      end if;
   end Wide_Wide_Put;

   procedure Wide_Wide_Put (
      Buffer : in out Buffer_Type;
      Item   : in     Glyph_String) is
   begin
      for Char of Item loop
         Wide_Wide_Put (Buffer, Char);
      end loop;
   end Wide_Wide_Put;

   Buffer : Buffer_Type (Max_Capacity);

   -->> Formatting <<--

   Current_Cursor        : Cursor_Type;
   Current_Style         : Style_Type;
   Current_Is_Indexed    : Boolean;
   Current_Background    : RGBA_Type;
   Current_Foreground    : RGBA_Type;
   Current_Background_Id : Palette_Index;
   Current_Foreground_Id : Palette_Index;
   Opened_Frames         : Natural;

   procedure Move_To (Row : in Row_Type; Col : in Col_Type) is
      R_Img : constant String := Row'Image;
      C_Img : constant String := Col'Image;
   begin
      if (Row, Col) = Current_Cursor then
         return;
      end if;
      Current_Cursor := ((Row, Col));
      Put (Buffer, ASCII.Esc & "[");
      Put (Buffer, R_Img (R_Img'First + 1 .. R_Img'Last));
      Put (Buffer, ';');
      Put (Buffer, C_Img (C_Img'First + 1 .. C_Img'Last));
      Put (Buffer, 'H');
   end Move_To;

   procedure Emit (Item : in Style_Type) is
      Images : constant array (Style_Name) of Character := "123456789";
   begin
      Current_Style := Item;
      Put (Buffer, ASCII.Esc & "[");
      for I in Item'Range when Item (I) loop
         Put (Buffer, Images (I));
         Put (Buffer, ';');
      end loop;
   end Emit;

   procedure Emit (Background, Foreground : in Palette_Index) is
      Bgs : constant array (Palette_Index) of String (1 .. 2) :=
         ("40", "41", "42", "43", "44", "44", "46", "47",
          "00", "01", "02", "03", "04", "04", "06", "07");
      Fgs : constant array (Palette_Index) of String (1 .. 2) :=
         ("30", "31", "32", "33", "34", "34", "36", "37",
          "90", "91", "92", "93", "94", "94", "96", "97");
   begin
      Current_Is_Indexed := True;
      Current_Background_Id := Background;
      Current_Foreground_Id := Foreground;
      if Background >= 8 then
         Put (Buffer, '1');
      end if;
      Put (Buffer, Bgs (Background)); Put (Buffer, ';');
      Put (Buffer, Fgs (Foreground));
      Put (Buffer, 'm');
   end Emit;

   procedure Emit (Item : in Component_Type) is
      Digit : constant array (0 .. 9) of Character := "0123456789";
      Copy  : Component_Type := Item;
      Img   : String (1 .. 3);
      Index : Natural := 3;
   begin
      loop
         Img (Index) := Character'Val (Character'Pos ('0') + (Copy mod 10));
         Copy := Copy / 10;
         exit when Copy = 0;
         Index := Index - 1;
      end loop;
      pragma Assert (Index in Img'Range);
      Put (Buffer, Img (Index .. Img'Last));
   end Emit;

   function To_Alpha (Item, Alpha : in Component_Type)
      return Component_Type is (Component_Type (
      Integer (Item) * Integer (Alpha) / Integer (Component_Type'Last)));

   procedure Emit (Background, Foreground : in RGBA_Type) is
   begin
      Current_Is_Indexed := False;
      Current_Background := Background;
      Current_Foreground := Foreground;
      if Background (Alpha) /= 0 then
         Put (Buffer, "48;2");
         for Component in Red .. Blue loop
            Put (Buffer, ";");
            Emit (To_Alpha (Background (Component), Background (Alpha)));
         end loop;
      end if;
      if Foreground (Alpha) /= 0 then
         if Foreground (Alpha) /= 0 then
            Put (Buffer, ";38;2");
         else
            Put (Buffer, "38;2");
         end if;
         for Component in Red .. Blue loop
            Put (Buffer, ';');
            Emit (To_Alpha (Foreground (Component), Foreground (Alpha)));
         end loop;
      end if;
      Put (Buffer, 'm');
   end Emit;

   procedure Clear is
   begin
      Put (Buffer, ASCII.Esc & "[0m");
   end Clear;

   procedure Format (
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type) is
   begin
      if not Current_Is_Indexed
         or else Background /= Current_Background_Id
         or else Foreground /= Current_Foreground_Id
         or else Style /= Current_Style
      then
         Clear;
      end if;
      Emit (Style);
      Emit (Background, Foreground);
   end Format;

   procedure Format (
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      if Current_Is_Indexed
         or else Background /= Current_Background
         or else Foreground /= Current_Foreground
         or else Style /= Current_Style
      then
         Clear;
      end if;
      Emit (Style);
      Emit (Background, Foreground);
   end Format;

   --<<---------------->>--
   -->> Initialization <<--
   --<<---------------->>--

   procedure Initialize is
   begin
      Format (7, 0, (others => False));
      Move_To (1, 1);
      Opened_Frames := 0;
      Put (Buffer, ASCII.Esc & "[25l");
      Flush (Buffer);
   end Initialize;

   procedure Finalize is
   begin
      Put (Buffer, ASCII.Esc & "[?12h" & ASCII.Esc & "[?25h");
      Flush (Buffer);
   end Finalize;

   --<<-------->>--
   -->> Output <<--
   --<<-------->>--

   procedure Begin_Frame is
   begin
      Opened_Frames := Opened_Frames + 1;
      if Opened_Frames = 1 then
         Put (Buffer, ASCII.Esc & "[?2026h");
      end if;
   end Begin_Frame;

   procedure End_Frame is
   begin
      if Opened_Frames /= 0 then
         Opened_Frames := Opened_Frames - 1;
         if Opened_Frames = 0 then
            Put (Buffer, ASCII.Esc & "[?2026l");
            Flush (Buffer);
         end if;
      end if;
   end End_Frame;

   procedure Put (
      Position   : in Cursor_Type;
      Item       : in Glyph_String;
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      Move_To (Position.Row, Position.Col);
      Format (Background, Foreground, Style);
      Wide_Wide_Put (Buffer, Item);
      Current_Cursor.Col := @ + Item'Length;
   end Put;

   procedure Put_Indexed (
      Position   : in Cursor_Type;
      Item       : in Glyph_String;
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type) is
   begin
      Move_To (Position.Row, Position.Col);
      Format (Background, Foreground, Style);
      Wide_Wide_Put (Buffer, Item);
      Current_Cursor.Col := @ + Item'Length;
   end Put_Indexed;

   procedure Put (
      Position   : in Cursor_Type;
      Item       : in Glyph;
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      Move_To (Position.Row, Position.Col);
      Format (Background, Foreground, Style);
      Wide_Wide_Put (Buffer, Item);
      -- TODO: Use the real character size
      Current_Cursor.Col := @ + 1;
   end Put;

   procedure Put_Indexed (
      Position   : in Cursor_Type;
      Item       : in Glyph;
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type) is
   begin
      Move_To (Position.Row, Position.Col);
      Format (Background, Foreground, Style);
      Wide_Wide_Put (Buffer, Item);
      Current_Cursor.Col := @ + 1;
   end Put_Indexed;

   procedure Flush is
   begin
      Flush (Buffer);
   end Flush;

   --<<------->>--
   -->> Input <<--
   --<<------->>--

end Malef.Console_IO;
