with Malef.Surfaces;
with Malef.Groups;
with Malef.Window;

procedure CMYK is

   use Malef.Groups;
   use type Malef.Col_Type;
   use type Malef.Row_Type;

   -- We can declare colours using HTML tags.

   Base_Colour    : constant Malef.RGBA_Type := "#FFF";
   Grey_Colour    : constant Malef.RGBA_Type := "#80808055";
   Cyan_Colour    : constant Malef.RGBA_Type := "#0FF5";
   Magenta_Colour : constant Malef.RGBA_Type := "#F0F5";
   Yellow_Colour  : constant Malef.RGBA_Type := "#FF05";

   Blocks : constant Malef.Glyph_String
      := "██  ██  ██  ██  ██  ██  ██  ██  ██  ██  ";

   -- We then declare a Group for 4 layers. The Layer function aks for a
   -- Group, a Surface or the size of the Surface. We are going to modify them
   -- afterwards, then we have to specify the size of the surfaces.

   CMYK_Group : Malef.Groups.Group (5) := [
      Layer (19, 41, (35, 35)),
      Layer (16, 32, (32, 32)),
      Layer (16, 32, (32, 48)),
      Layer (16, 32, (40, 40)),
      Layer (1,  16, (32, 32))
   ];

   -- We can use Ada 2022's new `renames' keyword to reference the different
   -- layers and modify them independently.

   Base    renames CMYK_Group.Set_Surface (1).Element;
   Cyan    renames CMYK_Group.Set_Surface (2).Element;
   Magenta renames CMYK_Group.Set_Surface (3).Element;
   Yellow  renames CMYK_Group.Set_Surface (4).Element;
   Text    renames CMYK_Group.Set_Surface (5).Element;

begin

   -- Fill the backgrounds with the basic colours.

   Base.Fill_Background (Base_Colour);
   Cyan.Fill_Background (Cyan_Colour);
   Magenta.Fill_Background (Magenta_Colour);
   Yellow.Fill_Background (Yellow_Colour);

   Base.Fill_Foreground (Grey_Colour);
   Text.Fill_Background ("#FFF");
   Text.Fill_Foreground ("#000");
   Text.Put (1, 1, "Mode = NORMAL");

   -- Write text on the different layers

   Base.Put (2, 2, "Base");
   for I in Malef.Row_Type range 4 .. 18 when I mod 2 = 0 loop
      Base.Put (I, 2, Blocks);
   end loop;
   Cyan.Put (2, 2, "Cyan");
   Magenta.Put (2, Magenta.Cols - 8, "Magenta");
   Yellow.Put (2, 2, "Yellow");

   -- Add them to the Window

   Malef.Window.Window.Set_Group (CMYK_Group);
   Malef.Window.Window.Display;

end CMYK;
