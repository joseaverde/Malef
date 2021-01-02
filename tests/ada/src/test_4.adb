with Malef;
with Malef.Colors;   use Malef.Colors;
with Malef.Surfaces;

procedure Test_4 is
   Surface : Malef.Surfaces.Surface_Type;
   Colours : Malef.Surfaces.Surface_Type;
   Col     : Malef.Col_Type; use type Malef.Col_Type;
begin

   Surface := Malef.Surfaces.Create(10, 10);

   Set_Foreground(Surface => Surface,
                  Color   => Palettes(Malef_Palette)(True, Green));
   Set_Background(Surface => Surface,
                  Color   => Palettes(Malef_Palette)(False, Green));

   Set_Foreground(Surface  => Surface,
                  From_Row => 2,
                  From_Col => 3,
                  To_Row   => 5,
                  To_Col   => 7,
                  Color    => Palettes(Malef_Palette)(True, Black));
   Set_Background(Surface  => Surface,
                  From_Row => 1,
                  From_Col => 2,
                  To_Row   => 4,
                  To_Col   => 6,
                  Color    => Palettes(Malef_Palette)(False, White));

   Set_Foreground(Surface => Surface,
                  Row     => 10,
                  Col     => 10,
                  Color   => Palettes(Malef_Palette)(True, Red));
   Set_Background(Surface => Surface,
                  Row     => 10,
                  Col     => 10,
                  Color   => Palettes(Malef_Palette)(True, Blue));


   Colours := Malef.Surfaces.Create(2, 16);
   Col := 1;
   for Brightness in Boolean'Range loop
      for Colour in Color_Kind'Range loop
         Set_Foreground(Surface => Colours,
                        Row     => 1,
                        Col     => Col,
                        Color   => Palettes(Malef_Palette)(Brightness,Colour));
         Col := Col + 1;
      end loop;
   end loop;

   Col := 1;
   for Brightness in Boolean'Range loop
      for Colour in Color_Kind'Range loop
         Set_Background(Surface => Colours,
                        Row     => 2,
                        Col     => Col,
                        Color   => Palettes(Malef_Palette)(Brightness,Colour));
         Col := Col + 1;
      end loop;
   end loop;

   Malef.Initialize;
   Malef.New_Page;
   Surface.Debug_Put;
   delay 2.0;
   Colours.Debug_Put;
   Malef.New_Page;
   Malef.Finalize;

end Test_4;
