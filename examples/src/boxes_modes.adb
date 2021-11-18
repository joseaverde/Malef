with Ada.Text_IO;
with Ada.Calendar;

with Malef;
with Malef.Boxes;
with Malef.Characters;
with Malef.Colors;
with Malef.Surfaces;
with Malef.Windows;

procedure Boxes_Modes is
   use type Ada.Calendar.Time;

   Window_Sprite : Malef.Surfaces.Surface_Type;
   Wall_Sprite   : Malef.Surfaces.Surface_Type;
   Door_Sprite   : Malef.Surfaces.Surface_Type;
   Person_Sprite : Malef.Surfaces.Surface_Type; -- It's an alien
   Ilumination   : Malef.Surfaces.Surface_Type;

   Person_Layer       :          Malef.Boxes.Layer_Type := 10;
   Wall_Layer         : constant Malef.Boxes.Layer_Type := 2;
   Window_Layer       : constant Malef.Boxes.Layer_Type := 3;
   Door_Layer         : constant Malef.Boxes.Layer_Type := 4;
   Ilumination_Layer  : constant Malef.Boxes.Layer_Type :=
      Malef.Boxes.Layer_Type'Last;

   The_Box : Malef.Boxes.Box_Type;

   Brown      : constant Malef.Color_Type := (150, 75, 25, 255);
   Dark_Brown : constant Malef.Color_Type := (100, 25,  0, 255);

   Best    : Duration := Duration'Last;
   Worst   : Duration := 0.0;
   Average : Duration := 0.0;
   Next    : Duration;
   Frame   : Natural := 0;
   Start   : Ada.Calendar.Time;
   Finish  : Ada.Calendar.Time;

   procedure Prepare_Window_Sprite is
   begin

      Window_Sprite := Malef.Surfaces.Create (10, 20);
      The_Box.Insert (Window_Sprite.Get_Reference, Window_Layer);

      for R in Malef.Row_Type range 2 .. 9 loop
         Window_Sprite.Put (Malef.Characters.From_UTF8 ("||"), (R, 10));
      end loop;
      for C in Malef.Col_Type range 3 .. 9 loop
         Window_Sprite.Put (Malef.Characters.From_UTF8 ("_"), (5, C));
         Window_Sprite.Put (Malef.Characters.From_UTF8 ("-"), (6, C));
      end loop;
      for C in Malef.Col_Type range 12 .. 18 loop
         Window_Sprite.Put (Malef.Characters.From_UTF8 ("_"), (5, C));
         Window_Sprite.Put (Malef.Characters.From_UTF8 ("-"), (6, C));
      end loop;
      Malef.Colors.Set_Background (
         Surface => Window_Sprite,
         Color   => Brown
      );
      Malef.Colors.Set_Background (
         Surface  => Window_Sprite,
         Color    => (80, 160, 200, 100),
         From_Row => 2,
         To_Row   => 9,
         From_Col => 3,
         To_Col   => 18
      );

      Window_Sprite.Set_Position (6, 6);

   end Prepare_Window_Sprite;


   procedure Prepare_Wall_Sprite is
      use type Malef.Row_Type, Malef.Col_Type;
   begin
      Wall_Sprite := Malef.Surfaces.Create (24, 80);
      The_Box.Insert (Wall_Sprite.Get_Reference, Wall_Layer);
      Malef.Colors.Set_Background (
         Surface => Wall_Sprite,
         Color   => (255, 200, 200, 255)
      );
      Malef.Colors.Set_Background (
         Surface  => Wall_Sprite,
         Color    => (0, 0, 0, 0),
         From_Row => 6,
         To_Row   => 6 + 10 - 1,
         From_Col => 6,
         To_Col   => 6 + 20 - 1
      );
   end Prepare_Wall_Sprite;


   procedure Prepare_Door_Sprite is
      use type Malef.Row_Coord, Malef.Col_Coord;
   begin
      Door_Sprite := Malef.Surfaces.Create (16, 20);
      The_Box.Insert (Door_Sprite.Get_Reference, Door_Layer);
      Malef.Colors.Set_Background (
         Surface => Door_Sprite,
         Color   => Brown
      );
      Malef.Colors.Set_Background (
         Surface  => Door_Sprite,
         Color    => (255, 255, 0, 255),
         From_Row => 8,
         To_Row   => 8,
         From_Col => 17,
         To_Col   => 18
      );
      Door_Sprite.Set_Position (24 - 16 + 1, 80 - 20 - 8);
   end Prepare_Door_Sprite;

   Person_Col : Malef.Col_Coord := 1;
   procedure Prepare_Person_Sprite is
      use type Malef.Row_Coord;
      Sprite : constant array (Positive range 1 .. 16) of String (1 .. 16) := (
         "     ____       ",
         "    /####\      ",
         "   |######|     ",
         "   |######/     ",
         "    \____/      ",
         "       |        ",
         "      /|\       ",
         "     / | \      ",
         "    /  |  \     ",
         "   /   |   \    ",
         "       |        ",
         "       |        ",
         "      / \       ",
         "     /   \      ",
         "    /     \     ",
         "   /       \    "
      );
   begin
      Person_Sprite := Malef.Surfaces.Create (16, 16);
      Person_Sprite.Clear;
      The_Box.Insert (Person_Sprite.Get_Reference, Person_Layer);
      for R in Sprite'Range loop
         for C in Sprite(R)'Range loop
            if Sprite (R)(C) = ' ' then
               Malef.Colors.Set_Cursor_Background (
                  Surface => Person_Sprite,
                  Color   => (0, 0, 0, 0)
               );
            elsif Sprite (R)(C) = '#' then
               Malef.Colors.Set_Cursor_Background (
                  Surface => Person_Sprite,
                  Color   => (0, 220, 100, 255)
               );
               Malef.Colors.Set_Cursor_Foreground (
                  Surface => Person_Sprite,
                  Color   => (0, 220, 100, 255)
               );
            else
               if R /= 1 then
                  Malef.Colors.Set_Cursor_Background (
                     Surface => Person_Sprite,
                     Color   => (0, 220, 100, 255)
                  );
                  Malef.Colors.Set_Cursor_Foreground (
                     Surface => Person_Sprite,
                     Color   => (0, 255, 255, 255)
                  );
               end if;
            end if;
            Person_Sprite.Put (
               Item     => Malef.Characters.From_UTF8 (
                  Malef.Characters.UTF8_String("" & Sprite(R)(C))),
               Position => (Malef.Row_Type(R), Malef.Col_Type(C)));
         end loop;
      end loop;
      Person_Sprite.Set_Position (24 - 16 + 1, 1);
   end Prepare_Person_Sprite;

   procedure Prepare_Ilumination is
   begin
      Ilumination := Malef.Surfaces.Create (24, 80);
      The_Box.Insert (Ilumination.Get_Reference, Ilumination_Layer);
      Malef.Colors.Set_Background (
         Surface => Ilumination,
         Color   => (0, 0, 0, 100)
      );
   end Prepare_Ilumination;


   function Quit return Boolean is
      Available : Boolean;
      Dummy     : Character;
   begin
      Ada.Text_IO.Get_Immediate (Dummy, Available);
      return Available and then Dummy = 'q';
   -- Available := Ada.Text_IO.End_Of_File (Ada.Text_IO.Standard_Input);
   -- if Available then
   --    Ada.Text_IO.Get_Immediate (Dummy);
   --    return Dummy = 'q';
   -- else
   --    return False;
   -- end if;
   end Quit;

   use type Malef.Row_Coord, Malef.Col_Coord, Malef.Boxes.Layer_Type;

   Speed : Malef.Col_Coord := +1;
   Alpha : Natural := 32;
   Diff  : Integer := +4;

begin

   Prepare_Window_Sprite;
   Prepare_Wall_Sprite;
   Prepare_Door_Sprite;
   Prepare_Person_Sprite;
   Prepare_Ilumination;

   The_Box.Update;

   Malef.Initialize;

   Malef.Windows.Main_Window.Insert (The_Box.Get_Reference, 1);
   Malef.Windows.Main_Window.Update;

   while not Quit loop
      Start := Ada.Calendar.Clock;

      Person_Sprite.Set_Position ((24 - 16 + 1, Person_Col));
      if Frame mod 1 = 0 then
         Person_Col := Person_Col + Speed;
         if Person_Col = 80 - 20 - 8 then
            The_Box.Move (Person_Layer, (if Person_Layer = 10 then 1 else 10));
            Person_Layer := (if Person_Layer = 10 then 1 else 10);
            Speed := -Speed;
         elsif Person_Col = 1 then
            Speed := -Speed;
         end if;
      end if;

      Malef.Colors.Set_Background (
         Surface => Ilumination,
         Color   => (0, 0, 0, Malef.Color_Component_Type(Alpha))
      );
      Alpha := Alpha + Diff;
      if Alpha >= 255 then
         Diff := -Diff;
         Alpha := 255;
      elsif Alpha <= 72 then
         Diff := -Diff;
         Alpha := 72;
      end if;

      The_Box.Clear;
      The_Box.Update;
      Malef.Windows.Main_Window.Update;
      Malef.Windows.Main_Window.Draw;

      delay 0.1;
      Finish := Ada.Calendar.Clock;
      Next := Finish - Start - 0.1;
      if Next > Worst then
         Worst := Next;
      end if;
      if Next < Best then
         Best := Next;
      end if;
      if Frame = 0 then
         Average := Next;
      else
         Average := (Average * Frame + Next) / (Frame + 1);
      end if;
      Frame := Frame + 1;
   end loop;

   Malef.Finalize;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Frames:" & Frame'Image);
   Ada.Text_IO.Put_Line ("Average:" & Duration'Image (1.0 / Average) & "FPS");
   Ada.Text_IO.Put_Line ("Best:" & Duration'Image (1.0 / Best) & "FPS");
   Ada.Text_IO.Put_Line ("Worst:" & Duration'Image (1.0 / Worst) & "FPS");

end Boxes_Modes;
