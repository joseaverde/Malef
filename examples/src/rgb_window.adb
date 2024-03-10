with Malef.Groups;
with Malef.System;
with Malef.Window;

procedure RGB_Window is
   use Malef.Groups;
   My_Group : Group := [Layer (10, 10, (1, 1), Opacity => 0.3),
                        Layer (10, 10, (1, 5), Opacity => 0.3),
                        Layer (10, 10, (5, 3), Opacity => 0.3)];
   Red   renames My_Group.Set_Surface (1).Element;
   Green renames My_Group.Set_Surface (2).Element;
   Blue  renames My_Group.Set_Surface (3).Element;
begin

   Malef.System.Initialize;      -- Initialize the subsystem

   Red.Fill_Background ("#FF0000");
   Green.Fill_Background ("#00FF00");
   Blue.Fill_Background ("#0000FF");

   Malef.Window.Window.Set_Group (My_Group);
   -- If you want to process the group as it is in a protected object you have
   -- to pass a function to the `Process_Group` procedure.
   Malef.Window.Window.Display;

   delay 5.0;
   Malef.System.Finalize;        -- Finalize the subsystem

end RGB_Window;
