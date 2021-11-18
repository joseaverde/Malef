with Malef;
with Malef.Boxes;
with Malef.Colors;
with Malef.Surfaces;
with Malef.Windows;

procedure CMYK is
   Base    : Malef.Surfaces.Surface_Type;
   Cyan    : Malef.Surfaces.Surface_Type;
   Magenta : Malef.Surfaces.Surface_Type;
   Yellow  : Malef.Surfaces.Surface_Type;

   Base_Colour    : constant Malef.Color_Type := (255, 255, 255, 255);
   Cyan_Colour    : constant Malef.Color_Type := (0, 255, 255, 85);
   Magenta_Colour : constant Malef.Color_Type := (255, 0, 255, 85);
   Yellow_Colour  : constant Malef.Color_Type := (255, 255, 0, 85);

   CMYK_Box : Malef.Boxes.Box_Type;
begin

   Malef.Initialize;

   Base := Malef.Surfaces.Create (19, 41);
   Cyan := Malef.Surfaces.Create (16, 32);
   Magenta := Malef.Surfaces.Create (16, 32);
   Yellow := Malef.Surfaces.Create (16, 32);

   Malef.Colors.Set_Background (Base, Base_Colour);
   Malef.Colors.Set_Background (Cyan, Cyan_Colour);
   Malef.Colors.Set_Background (Magenta, Magenta_Colour);
   Malef.Colors.Set_Background (Yellow, Yellow_Colour);

   Base.Set_Position (35, 35);
   Cyan.Set_Position (32, 32);
   Magenta.Set_Position (32, 48);
   Yellow.Set_Position (40, 40);

   CMYK_Box.Insert (Base.Get_Reference, 1);
   CMYK_Box.Insert (Cyan.Get_Reference, 2);
   CMYK_Box.Insert (Magenta.Get_Reference, 3);
   CMYK_Box.Insert (Yellow.Get_Reference, 4);

   CMYK_Box.Update;

   Malef.Windows.Main_Window.Insert (CMYK_Box.Get_Reference, 1);
   Malef.Windows.Main_Window.Update;
   Malef.Windows.Main_Window.Draw;

   Malef.Finalize;

end CMYK;
