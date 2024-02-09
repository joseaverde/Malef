with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;

with Malef.Palettes;
with Malef.Surfaces;
with Malef.System;
with Malef.Groups;
with Malef.Window;

procedure Palettes is

   -- This example will change when the widget toolkit is finished.
   -- For the time being it showcases how are the applications intended to
   -- look like once everything is put together.

   -- Transparent   : constant := 0;
   Primary_Bg    : constant := 1;
   -- Secondary_Bg  : constant := 2;
   Primary_Fg    : constant := 3;
   Secondary_Fg  : constant := 4;
   -- Primary       : constant := 5;
   -- Secondary     : constant := 6;
   Shadow_Colour : constant := 7;
   Unfocused     : constant := 8;

   Dark_Mode : constant Malef.Palettes.Palette_Type := (
      0 => "#0000",        -- Transparent
      1 => "#282828",      -- Primary Background Colour
      2 => "#505050",      -- Secondary Background Colour
      3 => "#F0F0F0",      -- Primary Foreground Colour
      4 => "#DCDCDC",      -- Secondary Foreground Colour
      5 => "#F05050",      -- Primary Colour
      6 => "#DC2828",      -- Secondary Colour
      7 => "#0007",        -- Shadow
      8 => "#00000050",    -- Unfocused
      others => "#0000");

   Light_Mode : constant Malef.Palettes.Palette_Type := (
      0 => "#0000",        -- Transparent
      1 => "#F0F0F0",      -- Primary Background Colour
      2 => "#DCDCDC",      -- Secondary Background Colour
      3 => "#282828",      -- Primary Foreground Colour
      4 => "#505050",      -- Secondary Foreground Colour
      5 => "#50F0DC",      -- Primary Colour
      6 => "#28C828",      -- Secondary Colour
      7 => "#28282877",    -- Shadow
      8 => "#28282850",    -- Unfocused
      others => "#0000");

   use type Malef.Col_Type;
   use type Malef.Row_Type;

   Dialog_Rows : constant := 9;
   Dialog_Cols : constant := 20;
   Window_Rows : constant := 56;
   Window_Cols : constant := 180;

   Half_R  : constant Malef.Row_Type := (Window_Rows - Dialog_Rows) / 2;
   Half_C  : constant Malef.Col_Type := (Window_Cols - Dialog_Cols) / 2;

   use Malef.Groups;

   Group : Malef.Groups.Group (4) := [
      1 => Layer (Window_Rows, Window_Cols, (1, 1)),
      2 => Layer (Window_Rows, Window_Cols, (1, 1)),
      3 => Layer (Dialog_Rows, Dialog_Cols, (Half_R + 1, Half_C + 1)),
      4 => Layer (Dialog_Rows, Dialog_Cols, (Half_R, Half_C))];

   Window  renames Group.Set_Surface (1).Element;
   Unfocus renames Group.Set_Surface (2).Element;
   Shadow  renames Group.Set_Surface (3).Element;
   Dialog  renames Group.Set_Surface (4).Element;
begin

   Malef.System.Initialize;
   Malef.System.Set_Title ("Malef :: Palettes");

   -->> Surface Construction <<--

   Dialog.Fill_Background (Primary_Bg);
   Dialog.Fill_Foreground (Primary_Fg);
   Dialog.Fill_Foreground ((1, 4), (1, 10), Secondary_Fg);
   Dialog.Put (1, 1, Block => [
      "╭─ Example ────────╮",
      "│                  │",
      "│ This is how the  │",
      "│ dialogs are      │",
      "│ intended to look │",
      "│ like in the      │",
      "│ final product.   │",
      "│                  │",
      "╰──────────────────╯"]);
   Shadow.Fill (' ');
   Shadow.Fill_Background (Shadow_Colour);

   Unfocus.Fill_Background (Unfocused);

   Window.Fill_Background (Primary_Bg);
   Window.Fill_Foreground (Primary_Fg);

   Malef.Window.Window.Set_Group (Group);

   -->> No Palette <<--

   Malef.Window.Window.Display;
   delay 1.0;

   -->> Dark Mode <<--

   Dialog.Set_Palette := Dark_Mode;
   Shadow.Set_Palette := Dark_Mode;
   Window.Set_Palette := Dark_Mode;
   Unfocus.Set_Palette := Dark_Mode;

   Malef.Window.Window.Display;
   delay 1.0;

   -->> Light Mode <<--

   Dialog.Set_Palette := Light_Mode;
   Shadow.Set_Palette := Light_Mode;
   Window.Set_Palette := Light_Mode;
   Unfocus.Set_Palette := Light_Mode;

   Window.Put (1, 1, "Light Mode");
   Malef.Window.Window.Display;
   delay 1.0;
   Window.Put (1, 1, "Dark Mode  ");

   -->> Benchmark <<--

   declare
      use Ada.Calendar;
      Times  : constant := 30;
      Start  : Time;
      Stop   : Time;
      First  : Duration;
      Second : Duration;
   begin

      -->> Update & Redraw <<--

      Start := Clock;
      for I in 1 .. Times loop
         Dialog.Set_Palette := Dark_Mode;
         Shadow.Set_Palette := Dark_Mode;
         Window.Set_Palette := Dark_Mode;
         Unfocus.Set_Palette := Dark_Mode;
         Malef.Window.Window.Display;
      end loop;
      Stop := Clock;
      First := Stop - Start;

      -->> Drawing <<--

      Start := Clock;
      for I in 1 .. Times loop
         Dialog.Set_Palette := Dark_Mode;
         Shadow.Set_Palette := Dark_Mode;
         Window.Set_Palette := Dark_Mode;
         Unfocus.Set_Palette := Dark_Mode;
         Group.Update;
         Malef.Window.Window.Display;
      end loop;
      Stop := Clock;
      Second := Stop - Start;

      -->> Results <<--
      delay 1.0;
      New_Line (20);

      Put ("Time per frame (Update and Redraw):");
      Put (Duration'Image (First / Duration (Times))); Put ("s");
      New_Line;

      Put ("Time per frame (Just Drawing):");
      Put (Duration'Image (Second / Duration (Times))); Put ("s");
      New_Line;

      Put ("Frames per Second (Update and Redraw):");
      Put (Duration'Image (Duration (Times) / First));
      New_Line;

      Put ("Frames per Second (Just Drawing):");
      Put (Duration'Image (Duration (Times) / Second));
      New_Line;
   end;

   Malef.System.Finalize;

end Palettes;
