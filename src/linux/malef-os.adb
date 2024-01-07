-------------------------------------------------------------------------------
--                                                                           --
--                          M A L E F - O S . A D B                          --
--                               ( L I N U X )                               --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   B O D Y                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2024 José Antonio Verde Jiménez  All Rights Reserved  --
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

with Ada.Interrupts;
with Ada.Interrupts.Names;
with Interfaces.C;
with GNAT.OS_Lib;
with Malef.Events;

package body Malef.OS is

   -- TODO: Search in $PATH for executable, or remove completely the stty
   -- and tput calls.

   procedure Get_Dimensions (
      Rows : out Positive_Row_Count;
      Cols : out Positive_Col_Count)
   is

      type winsize is
         record
            ws_row    : Interfaces.C.unsigned_short;
            ws_col    : Interfaces.C.unsigned_short;
            ws_xpixel : Interfaces.C.unsigned_short;
            ws_ypixel : Interfaces.C.unsigned_short;
         end record with
         Convention => C;
      -- This is the `winsize' type found in <sys/ioctl.h> header from the C
      -- programming language. It's used to store the terminal size.

      function Ioctl (
         Fd      : in     Interfaces.C.int;
         Request : in     Interfaces.C.unsigned_long;
         Struct  :    out winsize)
         return Interfaces.C.int with
         Import        => True,
         Convention    => C,
         External_Name => "ioctl";
      -- The Ioctl function is used to get the size of the terminal. It's
      -- imported from C and it isn't available on Windows, that's why we have
      -- a separate package.

      TIOCGWINSZ : constant Interfaces.C.unsigned_long := 16#5413#;

      Ws   : winsize;
      Temp : Interfaces.C.int;
   begin
      Temp := Ioctl (Fd      => 1,     -- Standard Output
                     Request => TIOCGWINSZ,
                     Struct  => Ws);

      Rows := Row_Type (Ws.ws_row);
      Cols  := Col_Type (Ws.ws_col);
   end Get_Dimensions;

   function Execute (Program, Parameter : in String)
      return Boolean
   is
      Param       : aliased String := Parameter;
      Exit_Status : Integer;
   begin
      GNAT.OS_Lib.Spawn (Program_Name           => Program,
                         Args                   => [Param'Unchecked_Access],
                         Output_File_Descriptor => GNAT.OS_Lib.Standout,
                         Return_Code            => Exit_Status);
      return Exit_Status = 0;
   end Execute;

   procedure Initialize is
   begin
      Ada.Interrupts.Attach_Handler (
         New_Handler => Malef.Events.Event_Handler.Update_Terminal_Size'Access,
         Interrupt   => Ada.Interrupts.Names.SIGWINCH);
      Malef.Events.Event_Handler.Update_Terminal_Size;
   end Initialize;

   procedure Finalize is
   begin
      Ada.Interrupts.Detach_Handler (Ada.Interrupts.Names.SIGWINCH);
   end Finalize;

   procedure Prepare is
      Dummy : Boolean with Unreferenced;
   begin
      if not Execute ("/usr/bin/stty", "-echo") then
         Dummy := Execute ("stty", "sane");
         raise Initialization_Error with
         "Couldn't initialize the terminal with `stty', check it is " &
         "available in $PATH";
      end if;
      if not Execute ("/usr/bin/tput", "civis") then
         Dummy := Execute ("stty", "echo");
         Dummy := Execute ("stty", "sane");
         raise Initialization_Error with
         "Couldn't initialize the terminal with `tput', check it is " &
         "avaiable in $PATH";
      end if;
   end Prepare;

   procedure Restore is
   begin
      if not Execute ("/usr/bin/stty", "echo") or else
         not Execute ("/usr/bin/stty", "sane") or else
         not Execute ("/usr/bin/tput", "cnorm")
      then
         raise Initialization_Error with
         "Couldn't finalize the terminal. If your terminal is broken " &
         "(characters don't echo when you type, the cursor doesn't blink...)" &
         " run `stty sane' or `stty echo' followed by `tput cnorm'";
      end if;
   end Restore;

end Malef.OS;
