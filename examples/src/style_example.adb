with Ada.Text_IO;
with Malef.Styles;

procedure Style_Example is
   use Malef.Styles;

   My_Style : constant Style
      := [Foreground => "#FF00FF"
        , Background => "#00FF00"
        , Border_Thickness => "thin"
        ];
begin
   Ada.Text_IO.Put_Line (My_Style'Image);
end Style_Example;
