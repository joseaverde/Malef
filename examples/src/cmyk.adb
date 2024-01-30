with Malef.Surfaces;
with Malef.System;
with Malef.Groups;
with Malef.Window;

procedure CMYK is
   Base_Colour    : constant Malef.RGBA_Type := "#FFF";
   Grey_Colour    : constant Malef.RGBA_Type := "#80808055";
   Cyan_Colour    : constant Malef.RGBA_Type := "#0FF5";
   Magenta_Colour : constant Malef.RGBA_Type := "#F0F5";
   Yellow_Colour  : constant Malef.RGBA_Type := "#FF05";

   use Malef.Groups;
   use type Malef.Col_Type;
   use type Malef.Row_Type;

   CMYK_Group : Malef.Groups.Group (4) := [
      Element (19, 41, (35, 35)),
      Element (16, 32, (32, 32)),
      Element (16, 32, (32, 48)),
      Element (16, 32, (40, 40))
   ];

   Base    renames CMYK_Group.Set_Surface (1).Element;
   Cyan    renames CMYK_Group.Set_Surface (2).Element;
   Magenta renames CMYK_Group.Set_Surface (3).Element;
   Yellow  renames CMYK_Group.Set_Surface (4).Element;

   Blocks : constant Malef.Glyph_String
      := "██  ██  ██  ██  ██  ██  ██  ██  ██  ██  ";
begin
   Malef.System.Initialize;

   Base.Fill_Background (Base_Colour);
   Cyan.Fill_Background (Cyan_Colour);
   Magenta.Fill_Background (Magenta_Colour);
   Yellow.Fill_Background (Yellow_Colour);

   Base.Fill_Foreground (Grey_Colour);

   Base.Put (2, 2, "Base");
   for I in Malef.Row_Type range 4 .. 18 when I mod 2 = 0 loop
      Base.Put (I, 2, Blocks);
   end loop;
   Cyan.Put (2, 2, "Cyan");
   Magenta.Put (2, Magenta.Cols - 8, "Magenta");
   Yellow.Put (2, 2, "Yellow");

   Malef.Window.Show (Base.all);
   delay 1.0;
   Malef.Window.Show (Cyan.all);
   delay 1.0;
   Malef.Window.Show (Magenta.all);
   delay 1.0;
   Malef.Window.Show (Yellow.all);
   delay 1.0;

   CMYK_Group.Update;

   Malef.Window.Show (CMYK_Group.See_Surface);
   delay 1.0;

   -- Malef.System.Main.Append (CMYK'Unchecked_Access, (1, 1));
   -- Malef.System.Main.Update;
   -- Malef.System.Main.Draw;

   Malef.System.Finalize;
end CMYK;
