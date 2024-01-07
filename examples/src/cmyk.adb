with Ada.Text_IO;
with Malef.Surfaces;
with Malef.System;
with Malef.Boxes;

procedure CMYK is
   Base    : aliased Malef.Surfaces.Surface (19, 41);
   Cyan    : aliased Malef.Surfaces.Surface (16, 32);
   Magenta : aliased Malef.Surfaces.Surface (16, 32);
   Yellow  : aliased Malef.Surfaces.Surface (16, 32);

   Base_Colour    : constant Malef.RGBA_Type := (255, 255, 255, 255);
   Cyan_Colour    : constant Malef.RGBA_Type := (0, 255, 255, 85);
   Magenta_Colour : constant Malef.RGBA_Type := (255, 0, 255, 85);
   Yellow_Colour  : constant Malef.RGBA_Type := (255, 255, 0, 85);

   CMYK_Box : Malef.Boxes.Box (4);
begin
   Malef.System.Initialize;

   Base.Fill_Background (Base_Colour);
   Cyan.Fill_Background (Cyan_Colour);
   Magenta.Fill_Background (Magenta_Colour);
   Yellow.Fill_Background (Yellow_Colour);

   Yellow.Background_Id (2, 2) := 10;

   Ada.Text_IO.Put_Line (Base'Image);
   Ada.Text_IO.Put_Line (Cyan'Image);
   Ada.Text_IO.Put_Line (Magenta'Image);
   Ada.Text_IO.Put_Line (Yellow'Image);

   CMYK_Box.Insert (1, Base'Unchecked_Access, (35, 35));
   CMYK_Box.Insert (2, Cyan'Unchecked_Access, (32, 32));
   CMYK_Box.Insert (3, Magenta'Unchecked_Access, (32, 48));
   CMYK_Box.Insert (4, Yellow'Unchecked_Access, (40, 40));
   CMYK_Box.Update;

   Ada.Text_IO.Put_Line (CMYK_Box'Image);

   -- Malef.System.Main.Append (CMYK'Unchecked_Access, (1, 1));
   -- Malef.System.Main.Update;
   -- Malef.System.Main.Draw;

   Malef.System.Finalize;
end CMYK;
