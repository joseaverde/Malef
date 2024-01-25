with Malef.Surfaces;
with Malef.System;
with Malef.Boxes;
with Malef.System;
with Malef.Window;

procedure CMYK is
   Base    : aliased Malef.Surfaces.Surface (19, 41);
   Cyan    : aliased Malef.Surfaces.Surface (16, 32);
   Magenta : aliased Malef.Surfaces.Surface (16, 32);
   Yellow  : aliased Malef.Surfaces.Surface (16, 32);

   Base_Colour    : constant Malef.RGBA_Type := (255, 255, 255, 255);
   Grey_Colour    : constant Malef.RGBA_Type := (128, 128, 128, 128);
   Cyan_Colour    : constant Malef.RGBA_Type := (0, 255, 255, 85);
   Magenta_Colour : constant Malef.RGBA_Type := (255, 0, 255, 85);
   Yellow_Colour  : constant Malef.RGBA_Type := (255, 255, 0, 85);

   use Malef.Boxes;
   use type Malef.Col_Type;
   use type Malef.Row_Type;

   CMYK_Box : Malef.Boxes.Box (4) := [
      1 => Item (Base'Unchecked_Access, (35, 35)),
      2 => Item (Cyan'Unchecked_Access, (32, 32)),
      3 => Item (Magenta'Unchecked_Access, (32, 48)),
      4 => Item (Yellow'Unchecked_Access, (40, 40))
   ];
   Blocks : constant Malef.Glyph_String
      := "██  ██  ██  ██  ██  ██  ██  ██  ██  ██  ";
begin
   Malef.System.Initialize;

   Base.Fill_Background (Base_Colour);
   Cyan.Fill_Background (Cyan_Colour);
   Magenta.Fill_Background (Magenta_Colour);
   Yellow.Fill_Background (Yellow_Colour);

   Base.Fill_Foreground (Grey_Colour);

   Base.Put (2, 2, "Base");
   for I in Malef.Row_Type range 4 .. 18 when I mod 2 = 0 loop
      Base.Put (I, 2, Blocks);
   end loop;
   Cyan.Put (2, 2, "Cyan");
   Magenta.Put (2, Magenta.Cols - 8, "Magenta");
   Yellow.Put (2, 2, "Yellow");

   Malef.Window.Show (Base);
   delay 1.0;
   Malef.Window.Show (Cyan);
   delay 1.0;
   Malef.Window.Show (Magenta);
   delay 1.0;
   Malef.Window.Show (Yellow);
   delay 1.0;

   CMYK_Box.Update;

   Malef.Window.Show (CMYK_Box.Constant_Surface);
   delay 1.0;

   -- Malef.System.Main.Append (CMYK'Unchecked_Access, (1, 1));
   -- Malef.System.Main.Update;
   -- Malef.System.Main.Draw;

   Malef.System.Finalize;
end CMYK;
