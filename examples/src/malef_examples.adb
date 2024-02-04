with Malef.Application; use Malef.Application;
with Malef.Dialogs;     use Malef.Dialogs;
with Malef.Labels;      use Malef.Labels;

procedure Malef_Examples is
begin
   Application.Add (
      New_Dialog (
         Widget => New_Label ("Hello, World"),
         Title  => "Malef -- Examples",
         Mode   => Windowed,
         Rows   => 10,
         Cols   => 40));
end Malef_Examples;
