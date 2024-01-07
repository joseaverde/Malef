package body Malef.Palettes with Pure is

   function Distance (Left, Right : in RGBA_Type)
      return Integer is (
      [for I in Component_Name'Range =>
         (Integer (Left (I)) - Integer (Right (I))) ** 2]'Reduce ("+", 0));

   function Nearest (
      Palette : in Palette_Type;
      Item    : in RGBA_Type)
      return Palette_Index
   is
      Id   : Palette_Index := 0;
      Best : Integer := Distance (Palette (0), Item);
      Test : Integer;
   begin
      for I in Palette'Range loop
         Test := Distance (Palette (I), Item);
         if Test < Best then
            Test := Best;
            Id := I;
         end if;
      end loop;
      return Id;
   end Nearest;

end Malef.Palettes;
