-------------------------------------------------------------------------------
--                                                                           --
--  MALEF - S Y S T E M S - R E S T O R E _ T E R M I N A L . SEPARATE. ADB  --
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

with Ada.Text_IO;
with GNAT.OS_Lib;
with Malef.Exceptions;
with Malef.Systems.Shared; use Malef.Systems.Shared;

separate (Malef.Systems)

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
      -- TODO: Use subsystems here.
      Ada.Text_IO.Put(ASCII.ESC & "[0m");

      if Exit_Status /= 0 then
         raise Malef.Exceptions.Initialization_Error with
         "Couldn't finalize the terminal, you could run `stty sane' if it's " &
         "broken. Or you could try: `stty echo' and `tput cnorm' .";
      end if;

   end Restore_Terminal;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
