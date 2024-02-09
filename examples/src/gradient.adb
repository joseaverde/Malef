with Malef;
with Malef.Surfaces;
with Malef.Palettes;
with Malef.Groups;
with Malef.Window;

procedure Gradient is

   Width  : constant := 20;
   Height : constant := 20;
   From   : constant Malef.RGBA_Type := (255, 0, 0, 255);
   To     : constant Malef.RGBA_Type := (0, 255, 0, 255);

   function Distance (Left, Right : in Malef.Cursor_Type)
      return Float is (
      abs (Float (Left.Row) - Float (Right.Row)) +
      abs (Float (Left.Col) - Float (Right.Col)));

   Pal_Surf : Malef.Surfaces.Surface (Height, Width);
   Col_Surf : Malef.Surfaces.Surface (Height, Width);

   procedure Fill_Gradient (
      Background : in out Malef.RGBA_Type;
      Foreground : in out Malef.RGBA_Type;
      Position   : in     Malef.Cursor_Type)
   is
      Init  : constant Float := Distance (Position, (1, 1));
      Fin   : constant Float := Distance (Position, (Height, Width));
      Color : constant Malef.RGBA_Type :=
         [for I in Malef.RGBA_Type'Range =>
            Malef.Component_Type (
              Float (From (I)) * (Init / (Init + Fin))
            + Float (To (I)) * (Fin / (Init + Fin)))];
   begin
      Background := Color;
      Foreground := Color;
   end Fill_Gradient;

   procedure Fill_Palette_Gradient (
      Background : in out Malef.Palette_Index;
      Foreground : in out Malef.Palette_Index;
      Position   : in     Malef.Cursor_Type)
   is
      pragma Unreferenced (Foreground);
      Bg, Fg : Malef.RGBA_Type;
   begin
      Fill_Gradient (Bg, Fg, Position);
      Background := Malef.Palettes.Nearest (Col_Surf.Get_Palette, Bg);
   end Fill_Palette_Gradient;

begin
   Col_Surf.Fill (Fill_Gradient'Access);
   Pal_Surf.Fill (Fill_Palette_Gradient'Access);
   Malef.Window.Window.Set_Group ([Malef.Groups.Layer (Col_Surf, (1, 1)),
                                   Malef.Groups.Layer (Pal_Surf, (1, Width))]);
   Malef.Window.Window.Display;
end Gradient;
