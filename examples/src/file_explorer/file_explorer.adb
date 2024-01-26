with Ada.Wide_Wide_Text_IO;      use Ada.Wide_Wide_Text_IO;
with Ada.Environment_Variables;
with File_Explorers;
with Malef.Surfaces;
with Malef.Widgets;

procedure File_Explorer is
   Max_Width  : constant := 80;
   Max_Height : constant := 24;
   The_Area   : constant Malef.Widgets.Draw_Area := ((1, 1),
                                                     (Max_Height, Max_Width));
   Home     : constant String := Ada.Environment_Variables.Value ("HOME");
   Surface  : Malef.Surfaces.Surface (Max_Height, Max_Width);
   Explorer : constant File_Explorers.File_Explorer_Widget
            := File_Explorers.New_File_Explorer (Home);
begin
   Explorer.On_Draw (Surface, The_Area);
   Put_Line (Surface'Wide_Wide_Image);
end File_Explorer;
