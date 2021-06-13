-------------------------------------------------------------------------------
--                                                                           --
--   M A L E F - S Y S T E M S - F I N A L I Z E . S E P A R A T E . A D B   --
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
with Malef.Systems.Shared; use Malef.Systems.Shared;
with Malef.Systems.Utils;

separate (Malef.Systems)
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

      -- We finalize the available colours and styles.
      Available_Styles := (others => False);
      Available_Colors := (Bit1 => True, others => False);
      Saved_Screen      := False;
      Line_Wrapping     := False;
      Cursor_Visibility := True;
      Loaded_Subsystems (Choose) := Loaded_Subsystems (None);

      -- We unload the libraries.
      Malef.Systems.Utils.Unload_Libraries;

   end Finalize;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
