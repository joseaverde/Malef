with Ada.Environment_Variables;
with File_Explorers;
with Malef.Surfaces;
with Malef.Widgets;
with Malef.Window;
with Malef.Groups;
with Malef.System;

procedure File_Explorer is
   Max_Width  : constant := 80;
   Max_Height : constant := 24;
   The_Area   : constant Malef.Widgets.Draw_Area := ((1, 1),
                                                     (Max_Height, Max_Width));
   Home     : constant String :=
      (if Ada.Environment_Variables.Exists ("HOME")
         then Ada.Environment_Variables.Value ("HOME")
         else "C:\");
   Surface  : Malef.Surfaces.Surface (Max_Height, Max_Width);
   Explorer : constant File_Explorers.File_Explorer_Widget
            := File_Explorers.New_File_Explorer (Home);
begin
   Malef.System.Initialize;
   Explorer.On_Draw (Surface, The_Area);
   Malef.Window.Window.Set_Group ([Malef.Groups.Layer (Surface)]);
   Malef.Window.Window.Display;
   delay 5.0;
   Malef.System.Finalize;
end File_Explorer;
