-------------------------------------------------------------------------------
--                                                                           --
--    M A L E F - P L A T F O R M - T E R M I N A L - O U T P U T . A D B    --
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

with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Malef.Platform.Generic_Buffer;
with Malef.Platform.Images;

package body Malef.Platform.Terminal.Output is

   -->> State <<--

   Current_Cursor        : Cursor_Type;
   Current_Style         : Style_Type;
   Current_Is_Indexed    : Boolean;
   Current_Background    : RGBA_Type;
   Current_Foreground    : RGBA_Type;
   Current_Background_Id : Palette_Index;
   Current_Foreground_Id : Palette_Index;
   Opened_Frames         : Natural;

   package Buffer is
      new Platform.Generic_Buffer (
      Capacity => 1024,
      Stream   => Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output)
      );

   -->> Formatting <<--

   procedure Escape is
   begin
      Buffer.Put (ASCII.ESC & '[');
   end Escape;

   procedure Move_To (
      Row : in Row_Type;
      Col : in Col_Type) is
   begin
      if (Row, Col) /= Current_Cursor then
         Current_Cursor := ((Row, Col));
         Escape;           Buffer.Put (Images.Image (Row));
         Buffer.Put (';'); Buffer.Put (Images.Image (Col)); Buffer.Put ('H');
      end if;
   end Move_To;

   function Emit (
      Item : in Style_Type)
      return Boolean
   is
      Images : constant array (Style_Name) of Character := "123456789";
      Any    : Boolean := False;
   begin
      Current_Style := Item;
      Escape;
      for I in Item'Range when Item (I) loop
         if Any then
            Buffer.Put (';');
         end if;
         Buffer.Put (Images (I));
         Any := True;
      end loop;
      return Any;
   end Emit;

   procedure Emit (
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Semicolon  : in Boolean)
   is
      Bgs : constant array (Palette_Index) of String (1 .. 2) :=
         ["40", "41", "42", "43", "44", "44", "46", "47",
          "00", "01", "02", "03", "04", "04", "06", "07"];
      Fgs : constant array (Palette_Index) of String (1 .. 2) :=
         ["30", "31", "32", "33", "34", "34", "36", "37",
          "90", "91", "92", "93", "94", "94", "96", "97"];
   begin
      Current_Is_Indexed := True;
      Current_Background_Id := Background;
      Current_Foreground_Id := Foreground;
      if Semicolon then
         Buffer.Put (';');
      end if;
      if Background >= 8 then
         Buffer.Put ('1');
      end if;
      Buffer.Put (Bgs (Background)); Buffer.Put (';');
      Buffer.Put (Fgs (Foreground)); Buffer.Put ('m');
   end Emit;

   function To_Alpha (Item, Alpha : in Component_Type)
      return Component_Type is (Component_Type (
      Integer (Item) * Integer (Alpha) / Integer (Component_Type'Last)));

   procedure Emit (
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Semicolon  : in Boolean) is
   begin
      Current_Is_Indexed := False;
      Current_Background := Background;
      Current_Foreground := Foreground;
      if Background (Alpha) /= 0 then
         if Semicolon then
            Buffer.Put (";48;2");
         else
            Buffer.Put ("48;2");
         end if;
         for Item in Red .. Blue loop
            Buffer.Put (";");
            Buffer.Put (Images.Image (To_Alpha (Background (Item),
                                                Background (Alpha))));
         end loop;
      end if;
      if Foreground (Alpha) /= 0 then
         if Foreground (Alpha) /= 0 or else Semicolon then
            Buffer.Put (";38;2");
         else
            Buffer.Put ("38;2");
         end if;
         for Item in Red .. Blue loop
            Buffer.Put (';');
            Buffer.Put (Images.Image (To_Alpha (Foreground (Item),
                                                Foreground (Alpha))));
         end loop;
      end if;
      Buffer.Put ('m');
   end Emit;

   procedure Clear is
   begin
      Buffer.Put (ASCII.ESC & "[0m");
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
      Emit (Background, Foreground, Emit (Style));
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
      Emit (Background, Foreground, Emit (Style));
   end Format;

   --<<---------------->>--
   -->> Implementation <<--
   --<<---------------->>--

   procedure Initialize is
   begin
      -- TODO: Call termios
      Format (7, 0, [others => False]);
      Move_To (1, 1);
      Opened_Frames := 0;
      Buffer.Put (ASCII.ESC & "[25l");
      Flush;
   end Initialize;

   procedure Finalize is
   begin
      -- TODO: Call termios
      Buffer.Put (ASCII.ESC & "[0m"     -- Clear format
                & ASCII.ESC & "[?12h"   -- Restore terminal
                & ASCII.ESC & "[?25h");
      Flush;
   end Finalize;

   procedure Begin_Frame is
   begin
      Opened_Frames := Opened_Frames + 1;
      if Opened_Frames = 1 then
         Buffer.Put (ASCII.ESC & "[?2026h");
      end if;
   end Begin_Frame;

   procedure End_Frame is
   begin
      if Opened_Frames /= 0 then
         Opened_Frames := Opened_Frames - 1;
         if Opened_Frames = 0 then
            Buffer.Put (ASCII.ESC & "[?2026l");
            Flush;
         end if;
      end if;
   end End_Frame;

   function Width (
      Item : in Glyph)
      return Col_Type is (
      (case Item is
         when Dbl => 2,
         when Bck => 0,
         when others => 1));

   function Width (
      Item : in Glyph_String)
      return Col_Type is (
      [for Char of Item => Width (Char)]'Reduce ("+", 0));

   procedure Put (
      Position   : in Cursor_Type;
      Item       : in Glyph_String;
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      Move_To (Position.Row, Position.Col);
      Format (Background, Foreground, Style);
      Buffer.Wide_Wide_Put (Item);
      Current_Cursor.Col := @ + Width (Item);
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
      Buffer.Wide_Wide_Put (Item);
      Current_Cursor.Col := @ + Width (Item);
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
      Buffer.Wide_Wide_Put (Item);
      -- TODO: Use the real character size
      Current_Cursor.Col := @ + Width (Item);
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
      Buffer.Wide_Wide_Put (Item);
      Current_Cursor.Col := @ + Width (Item);
   end Put_Indexed;

   procedure Flush is
   begin
      Buffer.Flush;
   end Flush;

   procedure Set_Title (
      Item : in Wide_Wide_String) is
   begin
      Buffer.Put (ASCII.ESC & "]0;");
      Buffer.Wide_Wide_Put (Item);
      Buffer.Put (ASCII.BEL);
   end Set_Title;

end Malef.Platform.Terminal.Output;
