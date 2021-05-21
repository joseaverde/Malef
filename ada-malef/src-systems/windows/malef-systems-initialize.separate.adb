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

separate (Malef.Systems)
   procedure Initialize is
   begin

      -- We prepare some components from the library.
      -- We first set the `Has_Been_Initialized' variable to True while we
      -- call some functions that require Malef to be initialised.
      Has_Been_Initialized := True;

      -- TODO: Get the system's palette.
      Malef.Colors.Set_Palette(Malef.Colors.Windows_10_Console);

      -- We restore the previous state of the `Has_Been_Initialize' variable.
      Has_Been_Initialized := False;

   -- TODO: Remove this.
   exception when others => raise Malef.Exceptions.Initialization_Error;
   end Initialize;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
