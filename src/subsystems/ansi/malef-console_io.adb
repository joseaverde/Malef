with Ada.Strings.Text_Buffers.Unbounded;
with Ada.Wide_Wide_Text_IO;

package body Malef.Console_IO is

   Max_Capacity : constant := 2048;

   task body Console is

      Buffer : Ada.Strings.Text_Buffers.Unbounded.Buffer_Type;
      Count  : Natural := 0;

      -->> Formatting <<--

      Current_Cursor        : Cursor_Type;
      Current_Style        : Style_Type;
      Current_Is_Indexed    : Boolean;
      Current_Background    : RGBA_Type;
      Current_Foreground    : RGBA_Type;
      Current_Background_Id : Palette_Index;
      Current_Foreground_Id : Palette_Index;

      procedure Emit (Item : in String) is
      begin
         Buffer.Put (Item);
      end Emit;

      procedure Glyph_Emit (Item : in Glyph_String) is
      begin
         for Char of Item loop
            if Glyph'Pos (Char) < 32 then
               Buffer.Wide_Wide_Put (" ");
            else
               Buffer.Wide_Wide_Put (Char & "");
            end if;
         end loop;
         Count := Count + Item'Length;
         Current_Cursor.Col := @ + 1;
      end Glyph_Emit;

      procedure Move_To (Row : in Row_Type; Col : in Col_Type) is
         R_Img : constant String := Row'Image;
         C_Img : constant String := Col'Image;
      begin
         if (Row, Col) = Current_Cursor then
            return;
         end if;
         Current_Cursor := ((Row, Col));
         Emit (ASCII.Esc & "[");
         Emit (R_Img (R_Img'First + 1 .. R_Img'Last));
         Emit (";");
         Emit (C_Img (C_Img'First + 1 .. C_Img'Last));
         Emit ("H");
      end Move_To;

      procedure Emit (Item : in Style_Type) is
         Images : constant array (Style_Name) of Character := "123456789";
      begin
         Current_Style := Item;
         Emit (ASCII.Esc & "[");
         for I in Item'Range when Item (I) loop
            Emit ("" & Images (I));
            Emit (";");
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
            Emit ("1");
         end if;
         Emit (Bgs (Background)); Emit (";");
         Emit (Fgs (Foreground));
         Emit ("m");
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
         Emit (Img (Index .. Img'Last));
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
            Emit ("48;2");
            for Component in Red .. Blue loop
               Emit (";");
               Emit (To_Alpha (Background (Component), Background (Alpha)));
            end loop;
         end if;
         if Foreground (Alpha) /= 0 then
            if Foreground (Alpha) /= 0 then
               Emit (";38;2");
            else
               Emit ("38;2");
            end if;
            for Component in Red .. Blue loop
               Emit (";");
               Emit (To_Alpha (Foreground (Component), Foreground (Alpha)));
            end loop;
         end if;
         Emit ("m");
      end Emit;

      procedure Clear is
      begin
         Emit (ASCII.Esc & "[0m");
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

      procedure Flush_It is
      begin
         Ada.Wide_Wide_Text_IO.Put (Buffer.Wide_Wide_Get);
         Count := 0;
      end Flush_It;

   begin
      Main_Loop : loop

         -- Wait for the start signal.

         select
            accept Start;
         or
            terminate;
         end select;

         Format (7, 0, (others => False));
         Move_To (1, 1);

         IO_Loop : loop

            select
               accept Start;
            or
               accept Stop;
               Flush_It;
               exit IO_Loop;
            or
               accept Put (
                  Position   : in Cursor_Type;
                  Item       : in Glyph_String;
                  Background : in RGBA_Type;
                  Foreground : in RGBA_Type;
                  Style      : in Style_Type)
               do
                  Move_To (Position.Row, Position.Col);
                  Format (Background, Foreground, Style);
                  Glyph_Emit (Item);
               end Put;
            or
               accept Put_Indexed (
                  Position   : in Cursor_Type;
                  Item       : in Glyph_String;
                  Background : in Palette_Index;
                  Foreground : in Palette_Index;
                  Style      : in Style_Type)
               do
                  Move_To (Position.Row, Position.Col);
                  Format (Background, Foreground, Style);
                  Glyph_Emit (Item);
               end Put_Indexed;
            or
               accept Flush;
               Flush_It;
            or
               terminate;
            end select;

            if Count > Max_Capacity then
               Flush_It;
            end if;

         end loop IO_Loop;

      end loop Main_Loop;

   end Console;

end Malef.Console_IO;
