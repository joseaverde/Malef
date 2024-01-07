with Malef.Grids;
with Malef.Texts;
with Malef.Dialogs;

procedure Simple_App is
   use Malef;
   Grid   : Grids.Grid (2, 2);
   Dialog : Dialogs.Dialog;
begin
   Grid := [[Texts.Create ("Hello, World"), Texts.Create ("Salut, le Monde !")],
            [Texts.Create ("¡Hola Mundo!"), Texts.Create ("こんにちは、世界！")]];
   Dialog.Add (Grid);
   Dialog.Show;
   Dialog.Run;
end Simple_App;
