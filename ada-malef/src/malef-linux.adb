-------------------------------------------------------------------------------
--                                                                           --
--                       M A L E F - L I N U X . A D B                       --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
--                                                                           --
-------------------------------------------------------------------------------
--     Copyright (c) 2020 José Antonio Verde Jiménez All Rights Reserved     --
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

with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Malef.Exceptions;

package body Malef.Linux is

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
      GNAT.OS_Lib.Spawn(Program_Name           => Stty_PATH,
                        Args                   => Arguments_Stty,
                        Output_File_Descriptor => GNAT.OS_Lib.Standout,
                        Return_Code            => Exit_Status);
      if Exit_Status /= 0 then
         -- In case the `stty' command fails, we try to fix any kind of damage
         -- received by the terminal with a `stty sane'. We also free the
         -- arguments and let the user know that there has been an error.
         GNAT.OS_Lib.Spawn(Program_Name           => Stty_PATH,
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
      GNAT.OS_Lib.Spawn(Program_Name           => Tput_PATH,
                        Args                   => Arguments_Tput,
                        Output_File_Descriptor => GNAT.OS_Lib.Standout,
                        Return_Code            => Exit_Status);
      if Exit_Status /= 0 then
         -- In case the `tput' command fails, we undo the operation done with
         -- `stty' with the oposite command and try to fix the terminal in any
         -- case with `stty sane'. We then free the arguments and raise an
         -- initialization error.
         GNAT.OS_Lib.Spawn(Program_Name           => Stty_PATH,
                           Args                   => Arguments_Fix_Stty,
                           Output_File_Descriptor => GNAT.OS_Lib.Standout,
                           Return_Code            => Exit_Status);

         GNAT.OS_Lib.Spawn(Program_Name           => Stty_PATH,
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
      GNAT.OS_Lib.Spawn(Program_Name           => Stty_PATH,
                        Args                   => Arguments_Stty,
                        Output_File_Descriptor => GNAT.OS_Lib.Standout,
                        Return_Code            => Exit_Status);

      if Exit_Status /= 0 then
         -- Couldn't finalize successfuly, thus we try to sane the terminal.
         GNAT.OS_Lib.Spawn(Program_Name           => Stty_PATH,
                           Args                   => Arguments_Stty_Sane,
                           Output_File_Descriptor => GNAT.OS_Lib.Standout,
                           Return_Code            => Exit_Status);
      end if;

      -- We run the last command `tput cnorm'.
      GNAT.OS_Lib.Spawn(Program_Name           => Tput_PATH,
                        Args                   => Arguments_Tput,
                        Output_File_Descriptor => GNAT.OS_Lib.Standout,
                        Return_Code            => Buffer);

      Exit_Status := Exit_Status + Buffer;
      Free;

      if Exit_Status /= 0 then
         raise Malef.Exceptions.Initialization_Error with
         "Couldn't finalize the terminal, you could run `stty sane' if it's " &
         "broken. Or you could try: `stty echo' and `tput cnorm' .";
      end if;

   end Restore_Terminal;



   function Get_Path (Programme_Name : String)
                      return String is
      PATH  : constant String := Ada.Environment_Variables.Value("PATH");
      First : Positive := PATH'First;
      Last  : Positive := PATH'First;

      Searcher  : Ada.Directories.Search_Type;
      Dir_Entry : Ada.Directories.Directory_Entry_Type;
   begin

      while Last <= PATH'Last loop
         if PATH(Last) = ':' or Last = PATH'Last then
            Last := Last - 1;
            Find_In_Path:
               declare
               begin
                  Ada.Directories.Start_Search(Search    => Searcher,
                                               Directory => PATH(First..Last),
                                               Pattern   => Programme_Name);
                  while Ada.Directories.More_Entries(Search => Searcher) loop
                     Ada.Directories.Get_Next_Entry(
                        Search          => Searcher,
                        Directory_Entry => Dir_Entry);
                     if Ada.Directories.Simple_Name(Dir_Entry) = Programme_Name
                     then
                        return PATH(First .. Last) & '/';
                     end if;
                  end loop;
               end Find_In_Path;
            Last  := Last + 1;
            First := Last + 1;
         end if;
         Last := Last + 1;
      end loop;

      -- Couldn't find the commands, we return this so the Windows part doesn't
      -- become stupid. No error is raised until real initialization.
      return "";

   end Get_Path;
            

   -- This procedure is separate so it doesn't get stupid in Windows.
   procedure Get_Terminal_Size (Rows : out Row_Type;
                                Cols : out Col_Type) is separate;


   procedure Set_Title (Name : String) is
   begin

      Ada.Text_IO.Put(ASCII.ESC & "]2;" & Name & Character'Val(16#07#));

   end Set_Title;

end Malef.Linux;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
