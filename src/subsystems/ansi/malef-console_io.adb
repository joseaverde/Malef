with Ada.Text_IO;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Interfaces.C;
with System;

package body Malef.Console_IO is

   package Unicode renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

   Max_Capacity  : constant := 128;

   type Buffer_Type (
      Capacity : Positive) is
      record
         Index : Natural := 0;
         Data  : String (1 .. Capacity);
      end record;

   function write (
      fd    : in Interfaces.C.int;
      buf   : in System.Address;
      count : in Interfaces.C.size_t)
      return Interfaces.C.size_t with
      Import        => True,
      Convention    => C,
      External_Name => "write";

      -- procedure Flush_It is

      --    Data : String := Buffer.Get_UTF_8;
      --    X : Interfaces.C.size_t with Unreferenced;
      -- begin
      --    -- Ada.Wide_Wide_Text_IO.Put (Buffer.Wide_Wide_Get);
      --    X := write (1, Data (Data'First)'Address, Data'Length);
      --    -- Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Data'Length'Image);
      --    -- Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, X'Image);
      -- end Flush_It;

   procedure Flush (
      Buffer : in out Buffer_Type)
   is
      C : constant Interfaces.C.size_t := Interfaces.C.size_t (Buffer.Index);
      X : Interfaces.C.size_t with Unreferenced;
   begin
      -- Ada.Text_IO.Put (Buffer.Data (1 .. Buffer.Index));
      X := write (1, Buffer.Data (Buffer.Data'First)'Address, C);
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
      Item   : in     Glyph_String) is
   begin
      for Char of Item loop
         if Glyph'Pos (Char) < 32 then
            Put (Buffer, ' ');
         else
            Put (Buffer, Unicode.Encode (Char & ""));
         end if;
      end loop;
   end Wide_Wide_Put;

   task body Console is

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
         Opened_Frames := 0;

         IO_Loop : loop

            select
               accept Start;
            or
               accept Stop;
               Flush (Buffer);
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
                  Wide_Wide_Put (Buffer, Item);
                  Current_Cursor.Col := @ + Item'Length;
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
                  Wide_Wide_Put (Buffer, Item);
                  Current_Cursor.Col := @ + Item'Length;
               end Put_Indexed;
            or
               accept Flush;
               Flush (Buffer);
            or
               accept Begin_Frame;
               Opened_Frames := Opened_Frames + 1;
               if Opened_Frames = 1 then
                  Put (Buffer, ASCII.Esc & "[?2026h");
               end if;
            or
               accept End_Frame;
               if Opened_Frames /= 0 then
                  Opened_Frames := Opened_Frames - 1;
                  if Opened_Frames = 0 then
                     Put (Buffer, ASCII.Esc & "[?2026l");
                     Flush (Buffer);
                  end if;
               end if;
            or
               terminate;
            end select;

         end loop IO_Loop;

      end loop Main_Loop;

   end Console;

end Malef.Console_IO;
