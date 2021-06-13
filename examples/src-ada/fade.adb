with Malef;
with Malef.Boxes;
with Malef.Colors;
with Malef.Surfaces;
with Malef.Windows;

procedure Fade is
   Surface : Malef.Surfaces.Surface_Type;
begin

   Surface := Malef.Surfaces.Create (32, 64);
   Malef.Windows.Main_Window.Insert (Surface.Get_Reference, 1);

   Malef.Initialize;

   for Alpha in reverse Malef.Color_Component_Type'Range loop
      Malef.Colors.Set_Background (Surface, (255, 255, 255, Alpha));
      Malef.Windows.Main_Window.Update;
      Malef.Windows.Main_Window.Draw;
      delay 0.01;
   end loop;

   Malef.Finalize;

end Fade;
