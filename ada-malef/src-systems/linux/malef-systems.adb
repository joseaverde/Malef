-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - S Y S T E M S . A D B                     --
--                               ( L I N U X )                               --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2021 José Antonio Verde Jiménez  All Rights Reserved  --
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
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Interfaces.C;
with Malef.Colors;
with Malef.Events;
with Malef.Exceptions;
with Malef.System_Utils;

package body Malef.Systems is

   type String_Access is access all String;

   procedure Free is new Ada.Unchecked_Deallocation(String, String_Access);

   Stty_Path : String_Access := new String'("/bin/stty");
   Tput_Path : String_Access := new String'("/bin/tput");

   --====-----------------------------------====--
   --====-- INITIALIZATION / FINALIZATION --====--
   --====-----------------------------------====--

   procedure Initialize is
      Found_Stty_Path : constant String := Malef.System_Utils.Get_Path(
                           Programme_Name                 => "stty",
                           PATH_Environment_Variable_Name => "PATH",
                           Default_PATHS                  => "/usr/bin:" &
                                                             "/bin");
      Found_Tput_Path : constant String := Malef.System_Utils.Get_Path(
                           Programme_Name                 => "tput",
                           PATH_Environment_Variable_Name => "PATH",
                           Default_PATHS                  => "/usr/bin:"&
                                                             "/bin");
   begin

      -- We seach the paths for `stty' and `tput'.
      if Found_Stty_Path = "" then
         raise Malef.Exceptions.Initialization_Error with
         "`stty' not found in PATH, couldn't initialize the terminal!";
      end if;

      if Found_Tput_Path = "" then
         raise Malef.Exceptions.Initialization_Error with
         "`tput' not found in PATH, couldn't initialize the terminal!";
      end if;

      if Stty_Path /= null then
         Free(Stty_Path);
      end if;

      if Tput_Path /= null then
         Free(Tput_Path);
      end if;

      Stty_Path := new String'(Found_Stty_Path & '/' & "stty");
      Tput_Path := new String'(Found_Tput_Path & '/' & "tput");

      -- We attach all the interrupt handlers.
      Ada.Interrupts.Attach_Handler(
         New_Handler => Malef.Events.Event_Handler.
                              Update_Terminal_Size'Access,
         Interrupt   => Ada.Interrupts.Names.SIGWINCH);

      -- Finally we prepare some components of the library.
      -- We set the `Has_Been_Initialized' variable to True temporarly so we
      -- can call some functions with private parts from the components.
      Has_Been_Initialized := True;

      -- TODO: Get the best colour for each distro.
      Malef.Colors.Set_Palette(Malef.Colors.xterm);

      -- We restore the `Has_Been_Initialized' variable.
      Has_Been_Initialized := False;

   end Initialize;


   procedure Finalize is
   begin

      -- We detach the event handlers.
      Ada.Interrupts.Detach_Handler(Interrupt=>Ada.Interrupts.Names.SIGWINCH);

      -- We free the PATHS.
      Free(Stty_Path);
      Free(Tput_Path);

      -- We finally change the components from the library back to a default
      -- state.
      Malef.Colors.Set_Palette(Malef.Colors.Malef_Palette);

   end Finalize;


   procedure Prepare_Terminal is
      Exit_Status    : Integer;
      Arguments_Stty : GNAT.OS_Lib.Argument_List
                     := (1 => new String'("-echo"));
      Arguments_Tput : GNAT.OS_Lib.Argument_List
                     := (1 => new String'("civis"));
      Arguments_Fix_Stty  : GNAT.OS_Lib.Argument_List
                          := (1 => new String'("echo"));
      Arguments_Stty_Sane : GNAT.OS_Lib.Argument_List
                          := (1 => new String'("sane"));

      procedure Free with Inline is
      begin
         for I in Arguments_Stty'Range loop
            GNAT.OS_Lib.Free(Arguments_Stty(I));
         end loop;

         for I in Arguments_Tput'Range loop
            GNAT.OS_Lib.Free(Arguments_Tput(I));
         end loop;

         for I in Arguments_Fix_Stty'Range loop
            GNAT.OS_Lib.Free(Arguments_Fix_Stty(I));
         end loop;

         for I in Arguments_Stty_Sane'Range loop
            GNAT.OS_Lib.Free(Arguments_Stty_Sane(I));
         end loop;
      end Free;
   begin

      -- We run `stty -echo'
      GNAT.OS_Lib.Spawn(Program_Name           => Stty_PATH.all,
                        Args                   => Arguments_Stty,
                        Output_File_Descriptor => GNAT.OS_Lib.Standout,
                        Return_Code            => Exit_Status);
      if Exit_Status /= 0 then
         -- In case the `stty' command fails, we try to fix any kind of damage
         -- received by the terminal with a `stty sane'. We also free the
         -- arguments and let the user know that there has been an error.
         GNAT.OS_Lib.Spawn(Program_Name           => Stty_PATH.all,
                           Args                   => Arguments_Stty_Sane,
                           Output_File_Descriptor => GNAT.OS_Lib.Standout,
                           Return_Code            => Exit_Status);
         Free;

         if Exit_Status /= 0 then
            raise Malef.Exceptions.Initialization_Error with
            "Fatal error, couldn't sane the terminal! Check `stty' is on " &
            "the PATH and rerun the programme!";
         end if;

         raise Malef.Exceptions.Initialization_Error with
         "Couldn't initialize the terminal with `stty' check it's on the " &
         "PATH!";
      end if;

      -- We then run `tput civis'
      GNAT.OS_Lib.Spawn(Program_Name           => Tput_PATH.all,
                        Args                   => Arguments_Tput,
                        Output_File_Descriptor => GNAT.OS_Lib.Standout,
                        Return_Code            => Exit_Status);
      if Exit_Status /= 0 then
         -- In case the `tput' command fails, we undo the operation done with
         -- `stty' with the oposite command and try to fix the terminal in any
         -- case with `stty sane'. We then free the arguments and raise an
         -- initialization error.
         GNAT.OS_Lib.Spawn(Program_Name           => Stty_PATH.all,
                           Args                   => Arguments_Fix_Stty,
                           Output_File_Descriptor => GNAT.OS_Lib.Standout,
                           Return_Code            => Exit_Status);

         GNAT.OS_Lib.Spawn(Program_Name           => Stty_PATH.all,
                           Args                   => Arguments_Stty_Sane,
                           Output_File_Descriptor => GNAT.OS_Lib.Standout,
                           Return_Code            => Exit_Status);

         Free;

         if Exit_Status /= 0 then
            raise Malef.Exceptions.Initialization_Error with
            "Fatal error, couldn't sane the terminal! Check `stty' is on " &
            "the PATH and rerun the programme!";
         end if;

         raise Malef.Exceptions.Initialization_Error with
         "Couldn't initialize the terminal with `tput', check it's on the " &
         "PATH!";
      end if;

      -- The operation succeeded and we can free the command arguments.
      -- The terminal is ready to run.
      Free;

   end Prepare_Terminal;


   procedure Restore_Terminal is
      Exit_Status    : Integer;
      Buffer         : Integer;
      Arguments_Stty : GNAT.OS_Lib.Argument_List
                     := (1 => new String'("echo"));
      Arguments_Tput : GNAT.OS_Lib.Argument_List
                     := (1 => new String'("cnorm"));
      Arguments_Stty_Sane : GNAT.OS_Lib.Argument_List
                          := (1 => new String'("sane"));

      procedure Free with Inline is
      begin
         for I in Arguments_Stty'Range loop
            GNAT.OS_Lib.Free(Arguments_Stty(I));
         end loop;

         for I in Arguments_Tput'Range loop
            GNAT.OS_Lib.Free(Arguments_Tput(I));
         end loop;

         for I in Arguments_Stty_Sane'Range loop
            GNAT.OS_Lib.Free(Arguments_Stty_Sane(I));
         end loop;
      end Free;
   begin

      -- We run `stty echo'
      GNAT.OS_Lib.Spawn(Program_Name           => Stty_PATH.all,
                        Args                   => Arguments_Stty,
                        Output_File_Descriptor => GNAT.OS_Lib.Standout,
                        Return_Code            => Exit_Status);

      if Exit_Status /= 0 then
         -- Couldn't finalize successfuly, thus we try to sane the terminal.
         GNAT.OS_Lib.Spawn(Program_Name           => Stty_PATH.all,
                           Args                   => Arguments_Stty_Sane,
                           Output_File_Descriptor => GNAT.OS_Lib.Standout,
                           Return_Code            => Exit_Status);
      end if;

      -- We run the last command `tput cnorm'.
      GNAT.OS_Lib.Spawn(Program_Name           => Tput_PATH.all,
                        Args                   => Arguments_Tput,
                        Output_File_Descriptor => GNAT.OS_Lib.Standout,
                        Return_Code            => Buffer);

      -- Finally we free everything and restore the original colour.
      Exit_Status := Exit_Status + Buffer;
      Free;
      Ada.Text_IO.Put(ASCII.ESC & "[0m");

      if Exit_Status /= 0 then
         raise Malef.Exceptions.Initialization_Error with
         "Couldn't finalize the terminal, you could run `stty sane' if it's " &
         "broken. Or you could try: `stty echo' and `tput cnorm' .";
      end if;

   end Restore_Terminal;



   --====------------------------------====--
   --====-- TERMINAL/CONSOLE CONTROL --====--
   --====------------------------------====--
   

   -- IDEA: Make it inline, call the best function that can return the Format
   --       Use Dim/Bright styles if needed.
   function Get_Format (Format : Format_Type)
                        return String is separate;


   procedure Get_Terminal_Size (Rows : out Row_Type;
                                Cols : out Col_Type) is
      --
      -- This is the `winsize' type found in <sys/ioctl.h> header from the C
      -- programming language. It's used to store the terminal size.
      --
      -- @field ws_row
      -- The number of rows the terminal has.
      --
      -- @field ws_col
      -- The number of columns the terminal has.
      --
      -- @field ws_xpixel
      -- TODO: Search information about this field.
      --
      -- @field ws_ypixel
      -- TODO: Search information about this field.
      --
      type winsize is
         record
            -- The number of rows the terminal has.
            ws_row    : Interfaces.C.unsigned_short;
            ws_col    : Interfaces.C.unsigned_short;
            ws_xpixel : Interfaces.C.unsigned_short;
            ws_ypixel : Interfaces.C.unsigned_short;
         end record
      with Convention => C;

      --
      -- The Ioctl function is used to get the size of the terminal. It's
      -- imported from C and it isn't available on Windows, that's why we have
      -- a separate package.
      --
      -- @param Fd
      -- It's the file descriptor: 1 is for standard output.
      --
      -- @param Request
      -- The request we are asking to IOCTL.
      --
      -- @param Struct
      -- The struct where the information will be retrieved.
      --
      function Ioctl (Fd      : Interfaces.C.int;
                      Request : Interfaces.C.unsigned_long;
                      Struct  : out Winsize)
                      return Interfaces.C.int;
      pragma Import (C, Ioctl, "ioctl");

      TIOCGWINSZ : constant Interfaces.C.unsigned_long := 16#5413#;

      Ws   : Winsize;
      Temp : Interfaces.C.int;
   begin

      Temp := Ioctl (Fd      => 1,
                     Request => TIOCGWINSZ,
                     Struct  => Ws);

      Rows := Row_Type(Ws.ws_row);
      Cols  := Col_Type(Ws.ws_col);

   end Get_Terminal_Size;


   procedure Set_Title (Name : String) is
   begin

      Ada.Text_IO.Put(ASCII.ESC & "]2;" & Name & Character'Val(16#07#));

   end Set_Title;


end Malef.Systems;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
