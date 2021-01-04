-------------------------------------------------------------------------------
--                                                                           --
--                    C _ M A L E F - E R R O R S . A D S                    --
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

with Ada.Exceptions;

--
-- @summary
--
--
-- @description
--
package C_Malef.Errors is

   pragma Elaborate_Body (C_Malef.Errors);

   procedure Push (Ada_Exception : Ada.Exceptions.Exception_Occurrence);

   procedure Pop;
   pragma Export (C, Pop, "malef_catchError");

   function Flying_Exception return bool;
   pragma Export (C, Flying_Exception, "malef_isFlyingError");

   function Get_Name return chars_ptr;
   pragma Export (C, Get_Name, "malef_getErrorName");

   function Get_Message return chars_ptr;
   pragma Export (C, Get_Message, "malef_getErrorMessage");

private

   Last_Exception : Ada.Exceptions.Exception_Occurrence;

end C_Malef.Errors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
