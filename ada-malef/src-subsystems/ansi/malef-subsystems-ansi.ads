-------------------------------------------------------------------------------
--                                                                           --
--             M A L E F - S U B S Y S T E M S - A N S I . A D S             --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
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

--
-- @summary
-- This package contains the functions for ANSI subsystem.
--
-- @description
-- This package contains a set of functions that will be dynamically loaded
-- using dynamic dispatching when the library is loaded. This functions will be
-- used by the System part of the library in order to use ANSI Escape Sequences
-- as the main way to control the terminal.
--
private package Malef.Subsystems.Ansi is

   type Subsystem is new Malef.Subsystems.Subsystem with null record;

   overriding
   function Get_Format (Subsys : not null access Subsystem;
                        Format : Format_Type)
                        return String;

end Malef.Subsystems.Ansi;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
