with Ada.Calendar;
with Ada.Text_IO;

with Malef;
with Malef.System;
with Malef.Palettes;
with Malef.Window;
with Malef.Groups;
with Benchmark_Configuration;

procedure Benchmark_Complex_Display is

   Step : Integer := 0;
   Max  : constant := 2_000;

   function Get_Index (Position : in Malef.Cursor_Type)
      return Integer is (
      (Step + Integer (Position.Row) + Integer (Position.Col)));

   procedure Colour_Filler (
      Background : in out Malef.RGBA_Type;
      Foreground : in out Malef.RGBA_Type;
      Position   : in     Malef.Cursor_Type)
   is
      Idx    : constant Integer := Get_Index (Position) mod 16;
      Bg_Idx : constant Malef.Palette_Index := Malef.Palette_Index (Idx);
      Fg_Idx : constant Malef.Palette_Index := Malef.Palette_Index (15 - Idx);
   begin
      Background := Malef.Palettes.Default_Palette (Bg_Idx);
      Foreground := Malef.Palettes.Default_Palette (Fg_Idx);
   end Colour_Filler;

   procedure Glyph_Filler (
      Item     : in out Malef.Glyph;
      Position : in     Malef.Cursor_Type)
   is
      Letters : constant Natural := 5 * 26;
      Idx  : constant Integer := Get_Index (Position) mod Letters;
      Char : constant Malef.Glyph := (
         if (Idx mod 26) < 5
            then Malef.Nul 
            else Malef.Glyph'Val (Malef.Glyph'Pos ('A') + Idx / 5));
   begin
      Item := Char;
   end Glyph_Filler;

   procedure Style_Filler (
      Item     : in out Malef.Style_Type;
      Position : in     Malef.Cursor_Type)
   is
      Count : constant Natural := 9;
      Idx   : constant Integer := Get_Index (Position) mod Count;
      Name  : constant Malef.Style_Name
         := Malef.Style_Name'Val (
               Malef.Style_Name'Pos (Malef.Style_Name'First) + Idx);
   begin
      Item := [others => True];
      Item (Name) := False;
   end Style_Filler;

   procedure Fill_It (
      Object : aliased in out Malef.Groups.Group)
   is
      Surface renames Object.Set_Surface (1).Element;
      Style : Malef.Style_Type;
   begin
      Surface.Fill (Colour_Filler'Access);
      Surface.Fill (Glyph_Filler'Access);
      Surface.Fill (Style_Filler'Access);
   end Fill_It;

   use type Ada.Calendar.Time;

   Start : Ada.Calendar.Time;
   Stop  : Ada.Calendar.Time;

begin
   Malef.System.Initialize;

   Malef.Window.Window.Set_Group ([Malef.Groups.Layer (
                                       Rows => Benchmark_Configuration.Rows,
                                       Cols => Benchmark_Configuration.Cols)]);
   Start := Ada.Calendar.Clock;
   for I in 1 .. Max loop
      Malef.Window.Window.Process_Group (Fill_It'Access);
      Malef.Window.Window.Display;
      Step := Step + 1;
   end loop;
   Stop := Ada.Calendar.Clock;

   Malef.System.Finalize;

   Ada.Text_IO.New_Line (4);
   Ada.Text_IO.Put_Line ("Elapsed time:" & Duration'Image (Stop - Start));
   Ada.Text_IO.Put_Line ("Frames per second:"
                        & Duration'Image (Duration (Max) / (Stop - Start)));
   Ada.Text_IO.Put_Line ("Seconds per frame:"
                        & Duration'Image ((Stop - Start) / Duration (Max)));

end Benchmark_Complex_Display;
