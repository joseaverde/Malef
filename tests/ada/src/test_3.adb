with Malef;
with Malef.Surfaces;

procedure Test_3 is
   My_Surface : Malef.Surfaces.Surface_Type;
begin

   My_Surface := Malef.Surfaces.Create (Rows => 10,
                                        Cols => 10);

   Malef.Initialize;

   Malef.New_Page;
   My_Surface.Debug_Put;

   Malef.Finalize;

end Test_3;
