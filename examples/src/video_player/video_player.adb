with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Video_Players;
with Malef.Surfaces;
with Malef.Window;
with Malef.System;

procedure Video_Player is
   use Video_Players;
   use Malef;

   Screen_Height : constant := 32;
   Screen_Width  : constant := 90;

   Player  : Video_Player_Widget;
   Surface : Surfaces.Surface (Screen_Height, Screen_Width);
begin

   Malef.System.Initialize;

   if Argument_Count = 0 then
      Put_Line (Standard_Error, "USAGE: `" & Command_Name & " [VIDEO]'");
      Set_Exit_Status (1);
      return;
   end if;

   Player := New_Video_Player (Argument (1));

   for I in 1 .. Player.Frame_Count loop
      Player.Next_Frame;
      Player.On_Draw (Surface, ((1, 1), (Screen_Height, Screen_Width)));
      Window.Show (Surface);
   end loop;

   Malef.System.Finalize;

end Video_Player;
