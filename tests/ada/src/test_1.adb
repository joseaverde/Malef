with Ada.Text_IO;
with Malef;
with Malef.Surfaces;

procedure Test_1 is
   Surface : Malef.Surfaces.Surface_Type;
begin

   Malef.Initialize (Info => Malef.Initialization_Information_Type'(
                              Operating_System  => Malef.GNU_Linux_OS,
                              Is_Ansi_Compliant => True,
                              Supported_Styles  => (others => True)));
   Surface.Debug_Put;
   Malef.Set_Title("TESTING");
   Ada.Text_IO.Put_Line("height =" & Malef.Get_Height'Image);
   Ada.Text_IO.Put_Line("width  =" & Malef.Get_Width 'Image);
   delay 2.0;

   Malef.Finalize;

end Test_1;
