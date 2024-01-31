with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Video_Players;
with Malef.Surfaces;
with Malef.Window;
with Malef.System;
with Malef.Groups;

procedure Video_Player is
   use Video_Players;
   use Malef;

   Screen_Height : constant := 32 * 2;
   Screen_Width  : constant := 90 * 2;

   Player      : Video_Player_Widget;
   Surface     : Surfaces.Surface (Screen_Height, Screen_Width);
   Transparent : Surfaces.Surface (Screen_Height, Screen_Width);

   procedure Update (Object : aliased in out Malef.Groups.Group) is
   begin
      Player.Next_Frame;
      Player.On_Draw (Object.Set_Surface (1),
                      ((1, 1), (Screen_Height, Screen_Width)));
   end Update;

begin

   Malef.System.Initialize;

   Transparent.Fill_Background ("#FF000030");
   Malef.Window.Window.Set_Group ([Malef.Groups.Layer (Surface)
                                 , Malef.Groups.Layer (Transparent)]);

   if Argument_Count = 0 then
      Put_Line (Standard_Error, "USAGE: `" & Command_Name & " [VIDEO]'");
      Set_Exit_Status (1);
      return;
   end if;

   Player := New_Video_Player (Argument (1));

   for I in 1 .. Player.Frame_Count loop
      Malef.Window.Window.Process_Group (Update'Access);
      Malef.Window.Window.Display;
   end loop;

   Malef.System.Finalize;

end Video_Player;
