-------------------------------------------------------------------------------
--                                                                           --
--           M A L E F - C O N S O L E _ I O - C O M M O N . A D B           --
--                                                                           --
--                                 M A L E F                                 --
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

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Task_Termination;
with Ada.Exceptions;
with Interfaces;
with Interfaces.C;
with System;

package body Malef.Console_IO.Common is

   use Interfaces;

   -- The GNAT standard library has a bug in the Ada.Wide_Text_IO and
   -- Ada.Wide_Wide_Text_IO packages. Mainly when you use the
   --
   --    procedure Get_Immediate (
   --       Item      : out Wide_Wide_Character;
   --       Available : out Boolean)
   --
   -- One would expect that if there is no character in the input buffer, it
   -- will terminate and put Available to False. However, it is only
   -- implemented for the Ada.Text_IO package, not for the Wide variantes. So
   -- it waits for input and then sets Available to True.
   --
   -- If you read the source code for the body of any of those packages
   -- (a-ztextio.adb) there is comment:
   --
   --  «Shouldn't we use getc_immediate_nowait here, like Text_IO???»
   --
   -- I'm going to patch the bug and report it to GCC. But for the time being
   -- I will be writing the implementation here. It will be removed in future
   -- versions when the patch is available upstream.
   --
   -- This implementation only works for UTF-8 for the time being.

   protected Termios is
      procedure Register_Task (
         Id : Ada.Task_Identification.Task_Id);
   private
      procedure Finalize (
         Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence);
   end Termios;

   protected body Termios is
      procedure Register_Task (
         Id : Ada.Task_Identification.Task_Id)
      is
         procedure Driver with
            Import        => True,
            Convention    => C,
            External_Name => "___c__malef__console_io__common__initialize";
      begin
         Ada.Task_Termination.Set_Specific_Handler (Id, Finalize'Access);
         Driver;
      end Register_Task;

      procedure Finalize (
         Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence)
      is
         procedure Driver with
            Import        => True,
            Convention    => C,
            External_Name => "___c__malef__console_io__common__finalize";
      begin
         Driver;
      end Finalize;
   end Termios;

   procedure Initialize_For_Task (
      Id : in Ada.Task_Identification.Task_Id) is
   begin
      Termios.Register_Task (Id);
   end Initialize_For_Task;

   type FILE is new System.Address;
   type ssize_t is new C.int;

   function ferror (
      stream : in FILE)
      return C.int with
      Import        => True,
      Convention    => C,
      External_Name => "ferror";

   function read (
      fd    : in C.int;
      buf   : in System.Address;
      count : in C.size_t)
      return ssize_t with
      Import        => True,
      Convention    => C,
      External_Name => "read";

   STDIN_FILENO : constant := 1;

   stdin : aliased constant FILE with
      Import        => True,
      Convention    => C,
      External_Name => "stdin";

   EOF_Ch : aliased constant Unsigned_8 with
      Import        => True,
      Convention    => C,
      External_Name => "___c__malef__console_io__common__eof_ch";

   procedure Get_Immediate_Step (
      Item : out Unsigned_8)
   is
      use type C.int;
      Buffer : aliased Unsigned_8;
      Count  : ssize_t;
   begin

      Item := 0;
      Count := read (STDIN_FILENO, Buffer'Address, 1);

      if Count = -1 then
         raise Termination_Error;
      elsif Count = 0 then
         raise Ada.IO_Exceptions.Device_Error;
      elsif Buffer = EOF_Ch then
         raise Ada.IO_Exceptions.End_Error;
      elsif ferror (stdin) /= 0 then
         raise Ada.IO_Exceptions.Device_Error;
      else
         Item := Buffer;
      end if;
   end Get_Immediate_Step;

   Masks : constant array (0 .. 4) of Unsigned_8 := [
         2#0011_1111#, 2#0111_1111#, 2#0001_1111#, 2#0000_1111#, 2#0000_0111#];

   subtype UTF_8_1_Byte is Unsigned_8 range 2#0000_0000# .. 2#0111_1111#;
   subtype UTF_8_Part   is Unsigned_8 range 2#1000_0000# .. 2#1011_1111#;
   subtype UTF_8_2_Byte is Unsigned_8 range 2#1100_0000# .. 2#1101_1111#;
   subtype UTF_8_3_Byte is Unsigned_8 range 2#1110_0000# .. 2#1110_1111#;
   subtype UTF_8_4_Byte is Unsigned_8 range 2#1111_0000# .. 2#1111_0111#;

   procedure Get_Immediate (
      Item : out Wide_Wide_Character)
   is
      Word : Unsigned_32 := 0;
      Byte : Unsigned_8;
      Mask : Unsigned_8;
      Size : Integer range 0 .. 4;
   begin
      Get_Immediate_Step (Byte);

      Size := (case Byte is when UTF_8_1_Byte => 1, when UTF_8_2_Byte => 2,
                            when UTF_8_3_Byte => 3, when UTF_8_4_Byte => 4,
                            when others => 0);
      Mask := Masks (Size);
      Word := Word or Unsigned_32 (Byte and Mask);
      Read_UTF_8_Sequence : for I in 2 .. Size loop
         Word := Shift_Left (Word, 6);
         Get_Immediate_Step (Byte);
         if Byte not in UTF_8_Part then
            Word := 0;
            exit Read_UTF_8_Sequence;
         end if;
         Word := Word or Unsigned_32 (Byte and Masks (0));
      end loop Read_UTF_8_Sequence;

      Item := Wide_Wide_Character'Val (Word);
   end Get_Immediate;

   task body Event_Feeder is
      Object : Malef.Console_IO.Event_Holders.Holder;
   begin
      loop
         Malef.Console_IO.Queue.Dequeue (Object);
         Process.all (Object.Constant_Reference.Element.all);
      end loop;
   end Event_Feeder;

   function Get_Interrupts
      return Interrupt_Array is
      separate;

   protected Event_Handler is

      procedure On_Resize;
      pragma Interrupt_Handler (On_Resize);

      procedure On_Cancel;
      pragma Interrupt_Handler (On_Cancel);

      procedure On_Kill;
      pragma Interrupt_Handler (On_Kill);

   end Event_Handler;

   protected body Event_Handler is

      pragma Warnings (Off,
         "potentially blocking operation in protected operation");
      procedure On_Resize is
         Size : Cursor_Type;
      begin
         Update_Dimensions;
         Get_Dimensions (Size.Row, Size.Col);
         Queue.Enqueue (Event_Holders.To_Holder (Events.Event_Type'(
            New_Size => Size,
            Time     => From_Start,
            Name     => Events.Resize_Event)));
      end On_Resize;

      procedure On_Cancel is
      begin
         Queue.Enqueue (Event_Holders.To_Holder (Events.Event_Type'(
            Time => From_Start,
            Name => Events.Cancel_Event)));
      end On_Cancel;

      procedure On_Kill is
      begin
         Queue.Enqueue (Event_Holders.To_Holder (Events.Event_Type'(
            Time => From_Start,
            Name => Events.Kill_Event)));
      end On_Kill;
      pragma Warnings (On,
         "potentially blocking operation in protected operation");

   end Event_Handler;

   Old_Handlers : array (Interrupt_Name)
                  of Ada.Interrupts.Parameterless_Handler;
   New_Handlers : constant array (Interrupt_Name)
                  of Ada.Interrupts.Parameterless_Handler
                  := [Resize_Id => Event_Handler.On_Resize'Access
                    , Cancel_Id => Event_Handler.On_Cancel'Access
                    , Kill_Id   => Event_Handler.On_Kill'Access
                    ];

   procedure Setup_Interrupts is
      use type Ada.Interrupts.Interrupt_Id;
      Ids : constant Interrupt_Array := Get_Interrupts;
   begin
      for Kind in Interrupt_Name when Ids (Kind) /= No_Id loop
         Ada.Text_IO.Put_Line (Kind'Image);
         Ada.Interrupts.Exchange_Handler (
            Old_Handler => Old_Handlers (Kind),
            New_Handler => New_Handlers (Kind),
            Interrupt   => Ids (Kind));
      end loop;
   end Setup_Interrupts;

   procedure Clear_Interrupts is
      use type Ada.Interrupts.Interrupt_Id;
      Ids : constant Interrupt_Array := Get_Interrupts;
   begin
      for Kind in Interrupt_Name when Ids (Kind) /= No_Id loop
         Ada.Interrupts.Attach_Handler (Old_Handlers (Kind), Ids (Kind));
      end loop;
   end Clear_Interrupts;

   Start_Time : Ada.Calendar.Time;

   procedure Start_Timer is
   begin
      Start_Time := Ada.Calendar.Clock;
   end Start_Timer;

   function From_Start return Duration is (
      Ada.Calendar."-" (Ada.Calendar.Clock, Start_Time));

end Malef.Console_IO.Common;
