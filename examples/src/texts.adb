with Malef;
with Malef.System;
with Malef.Labels;
with Malef.Surfaces;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

procedure Texts is
   use type Malef.Row_Type;
   use type Malef.Col_Type;
   Height : constant := 10;
   Width  : constant := 20;
   Top    : constant Malef.Glyph_String :=
      '╭' & [for I in 2 .. Width - 1 => '─'] & '╮';
   Middle : constant Malef.Glyph_String :=
      '│' & [for I in 2 .. Width - 1 => ' '] & '│';
   Bottom : constant Malef.Glyph_String :=
      '╰' & [for I in 2 .. Width - 1 => '─'] & '╯';
   Block  : constant Malef.Glyph_Block :=
      [for R in 1 .. Height =>
         [for C in 1 .. Width =>
            (if R = 1         then Top (C)
             elsif R = Height then Bottom (C)
             else                  Middle (C))]];
   Surface : Malef.Surfaces.Surface (Height, Width);
   Text    : constant Malef.Labels.Label_Widget :=
      Malef.Labels.New_Label ("Hello, World! ¡Hola Mundo! Bonjour le monde !");
begin

   Malef.System.Initialize;

   Surface.Fill_Background ((60, 60, 60, 255));
   Surface.Fill_Foreground ((160, 160, 160, 255));
   Surface.Put (1, 1, Block);

   -->> Left Aligned <<--

   Put_Line ("Left Aligned");
   Text.On_Draw (Surface, ((2, 2), (Height - 1, Width - 1)));
   Put_Line (Surface'Wide_Wide_Image);
   New_Line;

   -- -->> Right Aligned <<--

   -- Put_Line ("Right Aligned");
   -- Text.Set_Alignment (Malef.Labels.Right_Aligned);
   -- Text.On_Draw (Surface, ((2, 2), (Height - 1, Width - 1)));
   -- Put_Line (Surface'Wide_Wide_Image);
   -- New_Line;

   delay 5.0;
   Malef.System.Finalize;

end Texts;
