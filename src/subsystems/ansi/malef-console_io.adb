-------------------------------------------------------------------------------
--                                                                           --
--                  M A L E F - C O N S O L E _ I O . A D S                  --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   S P E C                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2021-2024 José Antonio Verde Jiménez  All Rights Reserved  --
-------------------------------------------------------------------------------
-- This file is part of Malef.                                               --
--                                                                           --
-- This program is free software:  you  can redistribute it and/or modify it --
-- under  the terms  of the  GNU  General License  as published by the  Free --
-- Software  Foundation,  either  version 3  of  the  License,  or  (at your --
-- opinion) any later version.                                               --
--                                                                           --
-- This  program  is distributed  in the  hope that  it will be  useful, but --
-- WITHOUT   ANY   WARRANTY;   without   even  the   implied   warranty   of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.                                          --
--                                                                           --
-- You should have received  a copy of the  GNU General Public License along --
-- with this program. If not, see <https://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Malef.Console_IO.Common;
with Malef.Debug;

package body Malef.Console_IO is

   pragma Unreserve_All_Interrupts;
   -- NOTE: This is needed for the Ctrl-C interrupt

   use Common;

   -- We can't use Ada.Text_IO to print the buffer, because for whatever reason
   -- printing UTF-8 with Ada.Text_IO outputs garbage if we compile with the
   -- `-gnatW8' flag (required for Wide_Wide_String s) on GNAT.
   --
   -- Also we can't use Ada.Wide_Wide_Text_IO, because it is slow as hell
   -- (maybe because it needs to transform the Unicode back to UTF-8).
   --
   -- So we had to find an native solution for this problem. Using the `write'
   -- system call is blazing fast. But it mey not be compatible with every
   -- operating sytem. Instead we use Streams, which yield a similar performace
   -- to the `write' sustem call, and they are in the standard library.

   package T_IO renames Ada.Text_IO;
   package T_IO_Streams renames T_IO.Text_Streams;

   Stream : constant T_IO_Streams.Stream_Access
          := T_IO_Streams.Stream (T_IO.Standard_Output);

   -- We need to keep a buffer to avoid writing the screen character by
   -- character (there are many single character calls). There is a buffer in
   -- the Ada.Text_IO.File_Type, but I'm not sure if the Stream package has it.
   -- I tested it on my lapto, and without a buffer, the code was 14 TIMES
   -- SLOWER. Therefore we are going to keep a buffer.
   --
   -- Also, as there will be only one buffer. We don't need to encapsulate it
   -- on a record and pass it everytime. Let's make it global and available
   -- for every function directly.

   Capacity : constant := 1024;
   Index    : Natural := 0;
   Data     : String (1 .. Capacity);

   procedure Flush is
   begin
      String'Write (Stream, Data (1 .. Index));
      Index := 0;
   end Flush;

   procedure Put (Item : in Character) is
   begin
      if Index = Capacity then
         Flush;
      end if;
      Index := @ + 1;
      Data (Index) := Item;
   end Put;

   procedure Put (Item : in String) is
   begin
      if Index + Item'Length > Capacity then
         Flush;
      end if;
      Data (Index + 1 .. Index + Item'Length) := Item;
      Index := @ + Item'Length;
   end Put;

   procedure Wide_Wide_Put (Item : in Glyph) is
   begin
      -- OPTIMISE: Search a function on character basis instead of strings.
      case Item is
         when   Nul  => Put (' ');
         when   Dbl  => null;
         when   Bck  => Put ("  ");
         when others => Put (Unicode.Encode (Item & ""));
      end case;
   end Wide_Wide_Put;

   procedure Wide_Wide_Put (Item : in Glyph_String) is
   begin
      for Char of Item loop
         Wide_Wide_Put (Char);
      end loop;
   end Wide_Wide_Put;

   -->> Formatting <<--

   Current_Cursor        : Cursor_Type;
   Current_Style         : Style_Type;
   Current_Is_Indexed    : Boolean;
   Current_Background    : RGBA_Type;
   Current_Foreground    : RGBA_Type;
   Current_Background_Id : Palette_Index;
   Current_Foreground_Id : Palette_Index;
   Opened_Frames         : Natural;

   procedure Move_To (Row : in Row_Type; Col : in Col_Type) is
      R_Img : constant String := Row'Image;
      C_Img : constant String := Col'Image;
   begin
      if (Row, Col) = Current_Cursor then
         return;
      end if;
      Current_Cursor := ((Row, Col));
      Put (ASCII.ESC & "[");
      Put (R_Img (R_Img'First + 1 .. R_Img'Last));
      Put (';');
      Put (C_Img (C_Img'First + 1 .. C_Img'Last));
      Put ('H');
   end Move_To;

   procedure Emit (Item : in Style_Type) is
      Images : constant array (Style_Name) of Character := "123456789";
   begin
      Current_Style := Item;
      Put (ASCII.ESC & "[");
      for I in Item'Range when Item (I) loop
         Put (Images (I));
         Put (';');
      end loop;
   end Emit;

   procedure Emit (Background, Foreground : in Palette_Index) is
      Bgs : constant array (Palette_Index) of String (1 .. 2) :=
         ["40", "41", "42", "43", "44", "44", "46", "47",
          "00", "01", "02", "03", "04", "04", "06", "07"];
      Fgs : constant array (Palette_Index) of String (1 .. 2) :=
         ["30", "31", "32", "33", "34", "34", "36", "37",
          "90", "91", "92", "93", "94", "94", "96", "97"];
   begin
      Current_Is_Indexed := True;
      Current_Background_Id := Background;
      Current_Foreground_Id := Foreground;
      if Background >= 8 then
         Put ('1');
      end if;
      Put (Bgs (Background)); Put (';');
      Put (Fgs (Foreground));
      Put ('m');
   end Emit;

   procedure Emit (Item : in Component_Type) is
      Copy  : Component_Type := Item;
      Img   : String (1 .. 3);
      Index : Natural := 3;
   begin
      loop
         Img (Index) := Character'Val (Character'Pos ('0') + (Copy mod 10));
         Copy := Copy / 10;
         exit when Copy = 0;
         Index := Index - 1;
      end loop;
      pragma Assert (Index in Img'Range);
      Put (Img (Index .. Img'Last));
   end Emit;

   function To_Alpha (Item, Alpha : in Component_Type)
      return Component_Type is (Component_Type (
      Integer (Item) * Integer (Alpha) / Integer (Component_Type'Last)));

   procedure Emit (Background, Foreground : in RGBA_Type) is
   begin
      Current_Is_Indexed := False;
      Current_Background := Background;
      Current_Foreground := Foreground;
      if Background (Alpha) /= 0 then
         Put ("48;2");
         for Component in Red .. Blue loop
            Put (";");
            Emit (To_Alpha (Background (Component), Background (Alpha)));
         end loop;
      end if;
      if Foreground (Alpha) /= 0 then
         if Foreground (Alpha) /= 0 then
            Put (";38;2");
         else
            Put ("38;2");
         end if;
         for Component in Red .. Blue loop
            Put (';');
            Emit (To_Alpha (Foreground (Component), Foreground (Alpha)));
         end loop;
      end if;
      Put ('m');
   end Emit;

   procedure Clear is
   begin
      Put (ASCII.ESC & "[0m");
   end Clear;

   procedure Format (
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type) is
   begin
      if not Current_Is_Indexed
         or else Background /= Current_Background_Id
         or else Foreground /= Current_Foreground_Id
         or else Style /= Current_Style
      then
         Clear;
      end if;
      Emit (Style);
      Emit (Background, Foreground);
   end Format;

   procedure Format (
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      if Current_Is_Indexed
         or else Background /= Current_Background
         or else Foreground /= Current_Foreground
         or else Style /= Current_Style
      then
         Clear;
      end if;
      Emit (Style);
      Emit (Background, Foreground);
   end Format;

   --<<---------------->>--
   -->> Initialization <<--
   --<<---------------->>--

   procedure Initialize_Input;
   procedure Finalize_Input;

   procedure Initialize_Output;
   procedure Finalize_Output;

   procedure Initialize is
   begin
      Initialize_Output;
      Initialize_Input;
   end Initialize;

   procedure Finalize is
   begin
      Finalize_Output;
      Finalize_Input;
   end Finalize;

   Event_Process : Event_Process_Type;

   procedure Register_Process (
      Process : in not null Event_Process_Type) is
   begin
      Event_Process := Process;
   end Register_Process;

   --<<-------->>--
   -->> Output <<--
   --<<-------->>--

   procedure Initialize_Output is
   begin
      Format (7, 0, [others => False]);
      Move_To (1, 1);
      Opened_Frames := 0;
      Put (ASCII.ESC & "[25l");
      Flush;
   end Initialize_Output;

   procedure Finalize_Output is
   begin
      Put (ASCII.ESC & "[0m"     -- Clear format
         & ASCII.ESC & "[?12h"   -- Restore terminal
         & ASCII.ESC & "[?25h");
      Flush;
   end Finalize_Output;

   procedure Begin_Frame is
   begin
      Opened_Frames := Opened_Frames + 1;
      if Opened_Frames = 1 then
         Put (ASCII.ESC & "[?2026h");
      end if;
   end Begin_Frame;

   procedure End_Frame is
   begin
      if Opened_Frames /= 0 then
         Opened_Frames := Opened_Frames - 1;
         if Opened_Frames = 0 then
            Put (ASCII.ESC & "[?2026l");
            Flush;
         end if;
      end if;
   end End_Frame;

   function Width (
      Item : in Glyph)
      return Col_Type is (
      (case Item is
         when Dbl => 2,
         when Bck => 0,
         when others => 1));

   function Width (
      Item : in Glyph_String)
      return Col_Type is (
      [for Char of Item => Width (Char)]'Reduce ("+", 0));

   procedure Put (
      Position   : in Cursor_Type;
      Item       : in Glyph_String;
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      Move_To (Position.Row, Position.Col);
      Format (Background, Foreground, Style);
      Wide_Wide_Put (Item);
      Current_Cursor.Col := @ + Width (Item);
   end Put;

   procedure Put_Indexed (
      Position   : in Cursor_Type;
      Item       : in Glyph_String;
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type) is
   begin
      Move_To (Position.Row, Position.Col);
      Format (Background, Foreground, Style);
      Wide_Wide_Put (Item);
      Current_Cursor.Col := @ + Width (Item);
   end Put_Indexed;

   procedure Put (
      Position   : in Cursor_Type;
      Item       : in Glyph;
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      Move_To (Position.Row, Position.Col);
      Format (Background, Foreground, Style);
      Wide_Wide_Put (Item);
      -- TODO: Use the real character size
      Current_Cursor.Col := @ + Width (Item);
   end Put;

   procedure Put_Indexed (
      Position   : in Cursor_Type;
      Item       : in Glyph;
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type) is
   begin
      Move_To (Position.Row, Position.Col);
      Format (Background, Foreground, Style);
      Wide_Wide_Put (Item);
      Current_Cursor.Col := @ + Width (Item);
   end Put_Indexed;

   procedure Set_Title (
      Item : in Wide_Wide_String) is
   begin
      Put (ASCII.ESC & "]0;");
      Wide_Wide_Put (Item);
      Put (ASCII.BEL);
   end Set_Title;

   --<<------->>--
   -->> Input <<--
   --<<------->>--

   -->> Terminal Dimensions <<--

   procedure Cursor_Position (
      Rows : out Positive_Row_Count;
      Cols : out Positive_Col_Count)
   -- This function gets the cursor position, it is only used once (on
   -- initialisation time). It needs to print to the screen and read
   -- immediately from it to get the cursor Position.
   --
   -- The:
   --    ESC & "[6n"
   --
   -- sends a request for a Device Status Report. The Device Status Report will
   -- write to standard input the following message:
   --
   --    ESC & "[" & Rows'Image & ";" & Cols'Image & "R";
   --
   -- Where Rows'Image and Cols'Image are the string representations of the
   -- Rows and Columns respectively (without the leading blank space Ada puts
   -- to integer images).
   --
   -- @param Rows
   -- The row the cursor is currently on will be written to this parameter.
   --
   -- @param Cols
   -- The column the cursor is currently on will be written to this parameter.
   is
      Char      : Character;
      Row       : Row_Count := 0;
      Col       : Col_Count := 0;
      Available : Boolean;
   begin

      -- Discard previous input

      loop
         Ada.Text_IO.Get_Immediate (Char, Available);
         exit when not Available;
      end loop;

      -- Device Status Report

      Ada.Text_IO.Put (ASCII.ESC & "[6n");
      Ada.Text_IO.Flush;
      Ada.Text_IO.Get_Immediate (Char); pragma Assert (Char = ASCII.ESC);
      Ada.Text_IO.Get_Immediate (Char); pragma Assert (Char = '[');

      -- Read Rows

      Ada.Text_IO.Get_Immediate (Char);
      pragma Assert (Char in '0' .. '9');
      loop
         Row := Row * 10 + Character'Pos (Char) - Character'Pos ('0');
         Ada.Text_IO.Get_Immediate (Char);
         exit when Char = ';';
         pragma Assert (Char in '0' .. '9');
      end loop;
      Rows := Row;

      -- Read Cols

      Ada.Text_IO.Get_Immediate (Char);
      pragma Assert (Char in '0' .. '9');
      loop
         Col := Col * 10 + Character'Pos (Char) - Character'Pos ('0');
         Ada.Text_IO.Get_Immediate (Char);
         exit when Char = 'R';
         pragma Assert (Char in '0' .. '9');
      end loop;
      Cols := Col;

   end Cursor_Position;

   procedure Terminal_Dimensions (
      Rows : out Positive_Row_Count;
      Cols : out Positive_Col_Count)
   -- This function gets the terminal dimensions (the number of rows and
   -- columns it has).
   --
   -- The idea is moving the Cursor as much to the bottom-right as possible
   -- (Row_Type'Last, Col_Type'Last) and getting the Cursor_Position. Terminals
   -- usually keep the cursor within the ranges of the screen and will return
   -- the dimensions of the terminal instead.
   --
   -- TODO: Use system call from: Malef.Systems.Get_Terminal_Size
   --       in:     Malef/ada-malef/src-systems/linux/
   --       commit: cbfdb88aeaa55fa1479519c9bbc7e5092c200947
   is
      R_Img : constant String := Row_Type'Last'Image;
      C_Img : constant String := Col_Type'Last'Image;
   begin
      Ada.Text_IO.Put (ASCII.ESC & "[");
      Ada.Text_IO.Put (R_Img (R_Img'First + 1 .. R_Img'Last));
      Ada.Text_IO.Put (';');
      Ada.Text_IO.Put (C_Img (C_Img'First + 1 .. C_Img'Last));
      Ada.Text_IO.Put ('H');
      Cursor_Position (Rows, Cols);
      Current_Cursor := (Row_Type'Last, Col_Type'Last);
   end Terminal_Dimensions;

   protected Shared_Input is

      procedure Get_Dimensions (
         Rows : out Positive_Row_Count;
         Cols : out Positive_Col_Count);

      procedure Update_Dimensions;

      procedure Set_Dimensions (
         Rows : in Positive_Row_Count;
         Cols : in Positive_Col_Count) with Unreferenced;

   private

      Shared_Rows : Positive_Row_Count;
      Shared_Cols : Positive_Col_Count;

   end Shared_Input;
   -- We encapsulate the terminal dimensions inside a protected object. We
   -- don't need to recalculate the position with the slow and complex
   -- Terminal_Dimensions function.

   protected body Shared_Input is

      procedure Get_Dimensions (
         Rows : out Positive_Row_Count;
         Cols : out Positive_Col_Count) is
      begin
         Rows := Shared_Rows;
         Cols := Shared_Cols;
      end Get_Dimensions;

      procedure Update_Dimensions is
      begin
         Terminal_Dimensions (Shared_Rows, Shared_Cols);
      end Update_Dimensions;

      procedure Set_Dimensions (
         Rows : in Positive_Row_Count;
         Cols : in Positive_Col_Count) is
      begin
         Shared_Rows := Rows;
         Shared_Cols := Cols;
      end Set_Dimensions;

   end Shared_Input;

   procedure Update_Dimensions is
   begin
      Shared_Input.Update_Dimensions;
   end Update_Dimensions;

   procedure Get_Dimensions (
      Rows : out Positive_Row_Count;
      Cols : out Positive_Col_Count) is
   begin
      Shared_Input.Get_Dimensions (Rows, Cols);
   end Get_Dimensions;

   -->> Input Events :: Keyboard <<--

   -- We cannot read from standard input from the main task, since we don't
   -- want the developer to worry about creating a new task. Instead we will
   -- have several tasks that will be running concurrently to get the input.
   --
   -- Note: Initially I wanted to use the Ada.Wide_Wide_Text_IO.Get_Immediate
   --       function. However, there is a bug in the implementation. Usually
   --       when you use the function is called with parameters:
   --
   --          procedure Get_Immediate (
   --             Item      : out Wide_Wide_Character;
   --             Available : out Boolean)
   --
   --       If there is not input available, the `Available' parameter would
   --       be set to False and continue without blocking. However, this does
   --       not happen. In fact if you read the implementation in file
   --       `a-ztextio.adb' you will see the following comment:
   --
   --          «Shouldn't we use getc_immediate_nowait here, like Text_IO???»
   --
   --       The same happens in the `Ada.Wide_Text_IO' package... Also when
   --       using `Ada.Text_IO' if I compile with `-gnatW8' it doesn't read
   --       the UTF-8 characters individually, it tries to convert them to
   --       `Character'. So if I write the japanese A (あ), it doesn't fit on
   --       a `Character' and raises an error. So that's out of the table.
   --
   --       I tried implementing my own function using the C function:
   --       `getc_immediate_nowait'. However there are two problems:
   --
   --       First of all, when you call the function it then sets up the
   --       terminal to avoid echoing (every single time). So when you do a
   --       `delay', the character will echo to the screen. Because you haven't
   --       called the Get_Immediate function yet. That means, you can't use
   --       the `Get_Immediate (Item, Available)' function.
   --
   --       Then, the worst of all was the task termination... I lost a whole
   --       weekend because of that. Basically if you run a different task with
   --       the `Get_Immediate' function such as:
   --
   --          task Input;
   --
   --          task body Input is
   --             Char : Wide_Wide_Character;
   --          begin
   --             loop
   --                Get_Immediate (Char);
   --                Queue.Enqueue (Event_Holders.To_Holder (
   --                   Events.Event_Type'(
   --                      Name => Keyboard_Event,
   --                      Key  => Key_Type (Char)));
   --             end loop;
   --          end Input;
   --
   --       When all the tasks terminate, this one won't terminate. If you try
   --       to abort it, it won't be terminated. Why? Because in the C
   --       implementatino of the `getc_immediate_common' function there is
   --       an infinite loop like:
   --
   --          while (!good) {
   --             nread = read(fd, &c, 1);
   --             if (nread > 0) {
   --                *ch = c;
   --                good = true;
   --             } else {
   --                good = false;
   --             }
   --          }
   --
   --       When you call abort, the `read' function terminates. However, you
   --       haven't read anything son either 0 or -1 is returned. Then `good'
   --       keeps being False and the loop is executed again. Then it gets
   --       blocked on the `read' function. This happens when `nread = -1'.
   --       The C function never finishes, and the task won't get to the
   --       rendez-vous point.
   --
   --       So... I implemented my own Get_Immediate function in the
   --       Malef.Console_IO.Common package that works just fine. I'll try to
   --       submit a patch to GCC fixing:
   --
   --        * `Ada.Wide_Text_IO.Get_Immediate (Item, Available);'
   --        * `Ada.Wide_Wide_Text_IO.Get_Immediate (Item, Available);'
   --        * `getc_immediate_common' to keep track of that edge case.
   --
   --       Even if those functions are fixed, I'll still have to use `termios'
   --       directly. Because I don't want characters to echo on the screen.

   task type Keyboard_Task is
   end Keyboard_Task;
   -- When the Keyboard is started it will start listening to standard input
   -- for key presses. No other function should use the Get_Immediate
   -- function as it will interfere with the Keyboard.

   task body Keyboard_Task is
      use Events;
      Key : Key_Type;
   begin
      Common.Initialize_For_Task (Keyboard_Task'Identity);
      loop
         Catch_End_Error : declare
         begin
            Common.Get_Immediate (Wide_Wide_Character (Key));
            if Key = Key_Type'Val (0) then
               Key := Key_Unknown;
            elsif Key = Key_Type'Val (27) then
               -- It is a sequence.
               Key := Key_Unknown;
            end if;
            Queue.Enqueue (Event_Holders.To_Holder (Events.Event_Type'(
                           Name => Events.Keyboard_Event,
                           Time => Common.From_Start,
                           Key  => Key)));
         exception
            when Common.End_Error =>
               Queue.Enqueue (Event_Holders.To_Holder (Events.Event_Type'(
                              Time => Common.From_Start,
                              Name => Events.Input_Closed)));
         end Catch_End_Error;
      end loop;
   end Keyboard_Task;

   -->> Input Events :: Master Thread <<--

   -- We need another thread: The Input_Task

   task Input_Task is
      entry Start;
      entry Stop;
   end Input_Task;
   -- This task will be responsible of starting and terminating other input
   -- tasks. So the program can finish and clean up even on unexpected
   -- conditions.

   task body Input_Task is
   begin

      loop

         -- We wait to be started, or we terminate without doing anything.

         select
            accept Start;
         or
            terminate;
         end select;

         -- Start other tasks and wait to be stopped.

         Common.Start_Timer;
         declare
            Keyboard : Keyboard_Task;
            Feeder   : Common.Event_Feeder (Event_Process);
         begin
            Common.Setup_Interrupts;
            select
               accept Stop;
            or
               terminate;
            end select;
            abort Keyboard;
            abort Feeder;
            Common.Clear_Interrupts;
         end;

         -- Dequeue everything from the original queue

         declare
            use type Ada.Containers.Count_Type;
            Object : Event_Holders.Holder;
         begin
            while Queue.Current_Use > 0 loop
               select
                  Queue.Dequeue (Object);
               or
                  delay 0.000_001;
               end select;
            end loop;
         end;

      end loop;

   end Input_Task;

   procedure Initialize_Input is
   begin
      Input_Task.Start;
   end Initialize_Input;

   procedure Finalize_Input is
   begin
      Input_Task.Stop;
   end Finalize_Input;

begin
   Shared_Input.Update_Dimensions;
end Malef.Console_IO;
