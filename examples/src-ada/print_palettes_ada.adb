with Ada.Text_IO;

with Malef;
with Malef.Colors;
with Malef.Surfaces;

procedure Print_Palettes_Ada is
   use type Malef.Col_Type;
   Surface : Malef.Surfaces.Surface_Type;
   Col     : Malef.Col_Type := 1;
   Ignore  : Character;
begin

   Surface := Malef.Surfaces.Create (
      Rows => 2,
      Cols => 16);

   Malef.Initialize;
   Malef.New_Page;

   for Pal in Malef.Colors.Palette_Kind'Range loop
      Malef.Colors.Set_Palette (Pal);
   -- Ada.Text_IO.Put_Line ( Malef.Colors.Get_Cursor_Foreground (Surface)(Malef.R)'Image ) ;
      Col := 1;
      for Bright in Boolean'Range loop
         for Colour in Malef.Colors.Color_Kind'Range loop
            Malef.Colors.Set_Foreground (
               Surface => Surface,
               Row     => 1,
               Col     => Col,
               Color   => Malef.Colors.Get_Color (Colour, Bright)
            );
            Malef.Colors.Set_Background (
               Surface => Surface,
               Row     => 2,
               Col     => Col,
               Color   => Malef.Colors.Get_Color (Colour, Bright)
            );
            Col := Col + 1;
         end loop;
      end loop;
      Surface.Debug_Put;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line (Pal'Image & "                                   ");
      Ada.Text_IO.Get_Immediate (Ignore);
   end loop;

   Malef.Finalize;

end Print_Palettes_Ada;
