with Malef;
with Malef.Boxes;
with Malef.Colors;   use Malef.Colors;
with Malef.Surfaces;
with Malef.Windows;
with Malef.Wrapper;

procedure Prog is
   Red_Window   : Malef.Surfaces.Surface_Type;
   Green_Window : Malef.Surfaces.Surface_Type;
   Blue_Window  : Malef.Surfaces.Surface_Type;
   RGB_Box      : Malef.Boxes.Box_Type;
   Palette      : Palette_Type;
begin

   Malef.Initialize;

   Palette := Get_Palette;

   Red_Window := Malef.Surfaces.Create (20, 40);
   Set_Background (Red_Window, Palette(True, Red));

   Green_Window := Malef.Surfaces.Create (10, 20);
   Set_Background (Green_Window, Palette(True, Green));

   Blue_Window := Malef.Surfaces.Create (15, 30);
   Set_Background (Blue_Window, Palette(True, Blue));

   RGB_Box.Insert (Red_Window.Get_Reference, 1);
   RGB_Box.Insert (Green_Window.Get_Reference, 20);
   RGB_Box.Insert (Blue_Window.Get_Reference, 10);
   RGB_Box.Update;

   Malef.Windows.Main_Window.Insert (RGB_Box.Get_Reference, 1);
   Malef.Windows.Main_Window.Update;
   Malef.Windows.Main_Window.Draw;

   Malef.Finalize;

end Prog;
