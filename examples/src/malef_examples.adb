with Malef.Application; use Malef.Application;
with Malef.Dialogs;     use Malef.Dialogs;
with Malef.Labels;      use Malef.Labels;

procedure Malef_Examples is
begin
   Application.Add (
      New_Dialog (
         New_Label ("Hello, World"),
         Mode => Windowed,
         Rows => 10,
         Cols => 40));
end Malef_Examples;

-- with Malef.Dialogs; use Malef.Dialogs;
-- with Malef.Widgets; use Malef.Widgets;
-- with Malef.Grids;   use Malef.Grids;
-- with Malef.Texts;   use Malef.Texts;
-- with Malfe.Buttons; use Malef.Buttons;
--
-- procedure Malef_Examples is
--
--    function When_Closed (Object : in out Widget'Class)
--       return Boolean;
--
--    Main : Dialog := New_Dialog (
--       Title       => "Malef -- Examples",
--       When_Closed => When_Closed'Access,
--       Widget      => Grid_Widget ([ [Text_Widget ("Welcome to Malef Examples"]
-- begin
--    null;
-- end Malef_Examples;
