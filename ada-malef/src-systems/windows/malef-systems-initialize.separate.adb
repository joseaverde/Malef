-------------------------------------------------------------------------------
--                                                                           --
-- M A L E F - S Y S T E M S - I N I T I A L I Z E . S E P A R A T E . A D B --
--                             ( W I N D O W S )                             --
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

with Malef.Colors;
with Malef.Events;
with Malef.Systems.Utils;

separate (Malef.Systems)
   procedure Initialize is
   begin

      Available_Styles := (others => True);
      Available_Colors := (others => True);
      -- We prepare some components from the library.
      -- We first set the `Has_Been_Initialized' variable to True while we
      -- call some functions that require Malef to be initialised.
      Has_Been_Initialized := True;

      Malef.Systems.Utils.Load_Libraries;
      Load_Libraries_Scope:
         declare
            use type Malef.Subsystems.Subsystem_Access;
            Check_Order : constant array (Positive range <>) of Subsystem_Kind
                        := (CMD, Ncurses, ANSI);
         begin

            for Subsys of Check_Order loop
               if Loaded_Subsystems (Subsys) /= null then
                  Loaded_Subsystems (Choose) := Loaded_Subsystems (Subsys);
                  goto Done_Load_Library_Scope;
               end if;
            end loop;

            raise Malef.Exceptions.Initialization_Error with
            "Couldn't initialise subsystems!";
            <<Done_Load_Library_Scope>>

         end Load_Libraries_Scope;
      -- TODO: Get the system's palette.
      Malef.Colors.Set_Palette(Malef.Colors.Windows_10_Console);
      Malef.Events.Event_Handler.Update_Terminal_Size;

      -- We restore the previous state of the `Has_Been_Initialize' variable.
      Has_Been_Initialized := False;

   end Initialize;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
