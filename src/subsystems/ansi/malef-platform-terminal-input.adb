-------------------------------------------------------------------------------
--                                                                           --
--     M A L E F - P L A T F O R M - T E R M I N A L - I N P U T . A D B     --
--                                                                           --
--                                 M A L E F                                 --
--                                  A N S I                                  --
--                                                                           --
--                              A D A   B O D Y                              --
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

with Ada.Unchecked_Deallocation;
with Interfaces;
with Interfaces.C;
with Malef.Platform.Events;
with Malef.Events;
with Malef.Platform.Generic_Get_Immediate;

package body Malef.Platform.Terminal.Input is

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

   task type Input_Task is
   end Input_Task;
   -- When the Keyboard is started it will start listening to standard input
   -- for key presses. No other function should use the Get_Immediate
   -- function as it will interfere with the Keyboard.

   type Input_Task_Access is access Input_Task;

   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Input_Task,
      Name   => Input_Task_Access);

   Input : Input_Task_Access := null;

   procedure Initialize is
      procedure Driver with
         Import        => True,
         Convention    => C,
         External_Name => "__malef__platform__terminal__input___c_prepare";
   begin
      Driver;
      Input := new Input_Task;
   end Initialize;

   procedure Finalize is
      procedure Driver with
         Import        => True,
         Convention    => C,
         External_Name => "__malef__platform__terminal__input___c_restore";
   begin
      abort Input.all;
      Free (Input);
      Driver;
   end Finalize;

   procedure Get_Byte_Immediate (
      Item : out Interfaces.Unsigned_8) is
      separate;

   procedure Get_Immediate is
      new Malef.Platform.Generic_Get_Immediate (
      Get_Byte_Immediate => Get_Byte_Immediate);

   task body Input_Task is
      use Malef.Events;
      use Malef.Platform.Events;
      Key : Key_Type;
   begin
      loop
         Catch_End_Error : declare
         begin
            Get_Immediate (Wide_Wide_Character (Key));
            if Key = Key_Type'Val (0) then
               Key := Key_Unknown;
            elsif Key = Key_Type'Val (27) then
               -- It is a sequence.
               Key := Key_Unknown;
            end if;
            Queue.Enqueue (+Event_Type'(Name => Keyboard_Event,
                                        Time => From_Start,
                                        Key  => Key));
         exception
            when End_Error =>
               Queue.Enqueue (+Event_Type'(Time => From_Start,
                                           Name => Input_Closed));
         end Catch_End_Error;
      end loop;
   end Input_Task;

   -->> Get Dimensions <<--

   type winsize is
      record
         ws_row    : Interfaces.C.unsigned_short;
         ws_col    : Interfaces.C.unsigned_short;
         ws_xpixel : Interfaces.C.unsigned_short;
         ws_ypixel : Interfaces.C.unsigned_short;
      end record with
      Convention => C;
   -- This struct is from <sys/ioctl.h> (<bits/ioctl-types.h>)

   function ioctl (
      fd      : in     Interfaces.C.int;
      request : in     Interfaces.C.unsigned_long;
      result  :    out winsize)
      return Interfaces.C.int with
      Import        => True,
      Convention    => C,
      External_Name => "ioctl";

   TIOCGWINSZ : constant Interfaces.C.unsigned_long := 16#5413#;

   procedure Get_Dimensions (
      Rows : out Positive_Row_Count;
      Cols : out Positive_Col_Count)
   is
      ws   : winsize;
      temp : Interfaces.C.int;
   begin
      temp := ioctl (1, TIOCGWINSZ, ws);
      Rows := Row_Type (ws.ws_row);
      Cols := Col_Type (ws.ws_col);
   end Get_Dimensions;

end Malef.Platform.Terminal.Input;
