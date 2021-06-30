-------------------------------------------------------------------------------
--                                                                           --
-- MALEF- S U B S Y S T E M S - C O M P O N E N T S - P U T _ U T I L S .ADS --
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

-- TODO: Comment it
-- @summary
--
--
-- @description
--
package Malef.Subsystems.Components.Put_Utils is

   Pass : exception;

   procedure Get_Bounds (Object : Shared_Surface_Access;
      In_Row     : out Row_Type;
      In_Col     : out Col_Type;
      From_Row   : out Row_Type;
      From_Col   : out Col_Type;
      The_Height : out Row_Type;
      The_Width  : out Col_Type)
      with Inline;


end Malef.Subsystems.Components.Put_Utils;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
