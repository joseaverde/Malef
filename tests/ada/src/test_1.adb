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

end Test_1;
