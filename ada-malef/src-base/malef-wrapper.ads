-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - W R A P P E R . A D S                     --
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
-- This function wraps another function defined by the user in order to restore
-- the terminal in case the user hasn't done it. That way we can be sure the
-- terminal isn't damaged after execution.
--
-- @param Parameters
-- The parameters the user has to enter (it's a record type).
--
-- @return
-- It returns another record type.
--
generic
   type Parameters_Type (<>) is private;
   type Return_Type is private;
   User_Function: access function (Parameters : Parameters_Type)
                                   return Return_Type;
function Malef.Wrapper (Parameters : Parameters_Type)
                        return Return_Type;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
