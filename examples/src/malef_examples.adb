with Malef.Application; use Malef.Application;
with Malef.Dialogs;     use Malef.Dialogs;
with Malef.Labels;      use Malef.Labels;

procedure Malef_Examples is
   LF : constant Wide_Wide_Character := Wide_Wide_Character'Val (10);
begin
   Application.Add (
      New_Dialog (
         Widget => New_Label ("Hello, World"         & LF &
                              "¡Hola Mundo!"         & LF &
                              "Bonjour, le Monde !"  & LF &
                              "Hallo Welt!"          & LF &
                              "こんにちは せかい!"   & LF &
                              "a b c d e f g h i j k l m n ñ o p q r s t " &
                              "u v w x y z"),
         Title  => "Malef -- Examples",
         Mode   => Windowed,
         Rows   => 10,
         Cols   => 40));
end Malef_Examples;
