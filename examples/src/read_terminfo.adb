with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Terminfo;

procedure Read_Terminfo is
   Term : Terminfo.Term_Type;
begin
   if Argument_Count = 0 then
      Put_Line ("Current terminal is located at: " & Terminfo.Search_Current);
   else
      for I in 1 .. Argument_Count loop
         Put ("Terminal "); Put (Argument (I));
         if Terminfo.Search (Argument (I)) = "" then
            Put (" couldn't be located");
         else
            Put (" is located at ");
            Put (Terminfo.Search (Argument (I))); New_Line;
            Terminfo.Open (Term, Terminfo.Search (Argument (I)));
            Put (Term'Image);
         end if;
         New_Line;
      end loop;
   end if;
end Read_Terminfo;
