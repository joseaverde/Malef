-------------------------------------------------------------------------------
--                                                                           --
--   MALEF-S Y S T E M S - P R E P A R E _ T E R M I N A L . SEPARATE. ADB   --
--                               ( L I N U X )                               --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
--                                                                           --
-------------------------------------------------------------------------------
--     Copyright (c) 2021 José Antonio Verde Jiménez All Rights Reserved     --
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

with GNAT.OS_Lib;
with Malef.Systems.Shared; use Malef.Systems.Shared;
with Malef.Exceptions;

separate (Malef.Systems)

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
            "the PATH and rerun the program!";
         end if;

         raise Malef.Exceptions.Initialization_Error with
         "Couldn't initialise the terminal with `stty' check it's on the " &
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
            "the PATH and rerun the program!";
         end if;

         raise Malef.Exceptions.Initialization_Error with
         "Couldn't initialize the terminal with `tput', check it's on the " &
         "PATH!";
      end if;

      -- The operation succeeded and we can free the command arguments.
      -- The terminal is ready to run.
      Free;

   end Prepare_Terminal;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
