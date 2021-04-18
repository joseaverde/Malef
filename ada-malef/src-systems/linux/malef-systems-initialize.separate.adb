-------------------------------------------------------------------------------
--                                                                           --
-- M A L E F - S Y S T E M S - I N I T I A L I Z E . S E P A R A T E . A D B --
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

with Ada.Interrupts;
with Ada.Interrupts.Names;
with Malef.Colors;
with Malef.Events;
with Malef.Exceptions;
with Malef.Systems.Shared; use Malef.Systems.Shared;
with Malef.Systems.Utils;

separate (Malef.Systems)
   procedure Initialize is

      Found_Stty_Path : constant String := Malef.Systems.Utils.Get_Path(
                           Program_Name                   => "stty",
                           PATH_Environment_Variable_Name => "PATH",
                           Default_PATHS                  => "/usr/bin:" &
                                                             "/bin");
      Found_Tput_Path : constant String := Malef.Systems.Utils.Get_Path(
                           Program_Name                   => "tput",
                           PATH_Environment_Variable_Name => "PATH",
                           Default_PATHS                  => "/usr/bin:"&
                                                             "/bin");
   begin

      -- We seach the paths for `stty' and `tput'.
      if Found_Stty_Path = "" then
         raise Malef.Exceptions.Initialization_Error with
         "`stty' not found in PATH, couldn't initialise the terminal!";
      end if;

      if Found_Tput_Path = "" then
         raise Malef.Exceptions.Initialization_Error with
         "`tput' not found in PATH, couldn't initialise the terminal!";
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

      -- TODO: We search for the available styles and colours.
      Available_Styles := (others => True);
      Available_Colors := (others => True);

      -- Finally we prepare some components of the library.
      -- We set the `Has_Been_Initialized' variable to True temporarly so we
      -- can call some functions with private parts from the components.
      Has_Been_Initialized := True;

      -- TODO: Get the best colour for each distro.
      Malef.Colors.Set_Palette(Malef.Colors.xterm);

      -- We prepare the libraries and choose the best library for the default
      -- one.
      Malef.Systems.Utils.Load_Libraries;
      Load_Libraries_Scope:
         declare
            use type Malef.Subsystems.Subsystem_Access;
            Check_Order : constant array (Positive range <>) of Subsystem_Kind
                        := (ANSI, Ncurses);
         begin

            for Subsys of Check_Order loop
               if Loaded_Subsystems (Subsys) /= null then
                  Loaded_Subsystems(Choose) := Loaded_Subsystems(Subsys);
                  goto Done_Load_Library_Scope;
               end if;
            end loop;

            raise Malef.Exceptions.Initialization_Error with
            "Couldn't initialise subsystems!";
            <<Done_Load_Library_Scope>>
         end Load_Libraries_Scope;

      -- We restore the `Has_Been_Initialized' variable.
      Has_Been_Initialized := False;

   end Initialize;



---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
