-------------------------------------------------------------------------------
--                                                                           --
--                M A L E F - S Y S T E M _ U T I L S . A D S                --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
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

--
-- @summary
-- This package contains functions used and shared by all systems.
--
-- @description
-- TODO: Give a better description.
--
package Malef.System_utils is

   --
   -- This function returns the directory where a programme is located
   -- searching in the given PATH.
   --
   -- @param Programme_Name
   -- The name of the programme/command to search.
   --
   -- @param PATH_Environment_Variable_Name
   -- The name of the environment variable where the PATHS are located: in
   -- linux it's under `PATH'.
   --
   -- @param Default_PATHS
   -- The paths to seach in if the variable isn't set.
   --
   -- @param Separator
   -- The PATH separator: in Linux it's `:' and `;' in Windows;
   function Get_Path (Programme_Name                 : String;
                      PATH_Environment_Variable_Name : String := "PATH";
                      Default_PATHS                  : String := "/bin";
                      Separator                      : Character := ':')
                      return String;

end Malef.System_utils;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
