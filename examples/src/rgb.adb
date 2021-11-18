with Malef;
with Malef.Boxes;
with Malef.Colors;
with Malef.Surfaces;
with Malef.Windows;

procedure RGB is
   Red     : Malef.Surfaces.Surface_Type;
   Green   : Malef.Surfaces.Surface_Type;
   Blue    : Malef.Surfaces.Surface_Type;
   RGB_Box : Malef.Boxes.Box_Type;

   Red_Colour   : constant Malef.Color_Type := (255, 0, 0, 85);
   Green_Colour : constant Malef.Color_Type := (0, 255, 0, 85);
   Blue_Colour  : constant Malef.Color_Type := (0, 0, 255, 85);
begin

   Red   := Malef.Surfaces.Create (5, 10);
   Green := Malef.Surfaces.Create (5, 10);
   Blue  := Malef.Surfaces.Create (5, 10);

   Red.Set_Position (1, 1);
   Green.Set_Position (1, 6);
   Blue.Set_Position (3, 3);

   -- https://docs.gimp.org/en/gimp-concepts-layer-modes.html
   -- Mode : Normal
   -- Dissolve
   -- Color_Erase
   -- Erase
   -- Merge
   -- Split

   Malef.Colors.Set_Background (Red, Red_Colour);
   Malef.Colors.Set_Background (Green, Green_Colour);
   Malef.Colors.Set_Background (Blue, Blue_Colour);

   RGB_Box.Insert (Red.Get_Reference, 1);
   RGB_Box.Insert (Green.Get_Reference, 2);
   RGB_Box.Insert (Blue.Get_Reference, 3);
   RGB_Box.Update;

   Malef.Initialize;

   Malef.Windows.Main_Window.Insert (RGB_Box.Get_Reference, 1);
   Malef.Windows.Main_Window.Update;
   Malef.Windows.Main_Window.Draw;

   Malef.Finalize;

end RGB;
