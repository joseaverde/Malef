with Ada.Text_IO;
with Malef.Styles;
with Malef.Styles.Compat;
with Malef.Styles.Dark;
with Malef.Styles.Light;

procedure Style_Example is
   use Malef.Styles;

   My_Style : constant Style
      := [Foreground => "#FF00FF"
        , Background => "#00FF00"
        , Border_Thickness => "thin"
        ];
begin
   Ada.Text_IO.Put_Line (My_Style'Image);
   Ada.Text_IO.Put_Line (Malef.Styles.Compat.Style'Image);
   Ada.Text_IO.Put_Line (Malef.Styles.Dark.Style'Image);
   Ada.Text_IO.Put_Line (Malef.Styles.Light.Style'Image);
end Style_Example;
