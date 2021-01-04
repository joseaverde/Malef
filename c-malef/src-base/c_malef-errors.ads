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
-- This package is used for error handling in C.
--
-- @description
-- Due to the lack of exception handling system in C, every function has been
-- wrapped and all the exception handled. That way we can ensure no unhandled
-- exception is given to C. This package aims to connect to the C library part
-- and communicate them the errors found. (Remember to free the strings).
--
package C_Malef.Errors is

   pragma Elaborate_Body (C_Malef.Errors);

   --
   -- This function pushes an exception to the Last_Exception.
   --
   -- @param Ada_Exception
   -- The exception to push.
   --
   procedure Push (Ada_Exception : Ada.Exceptions.Exception_Occurrence);

   --
   -- This procedure removes the last exception.
   --
   procedure Pop;
   pragma Export (C, Pop, "malef_catchError");

   --
   -- This function checks whether there is an exception pushed.
   --
   -- @return
   -- Whether an non-catched exception has been raised.
   --
   function Flying_Exception return bool;
   pragma Export (C, Flying_Exception, "malef_isFlyingError");

   --
   -- This function returns an ALLOCATED pointer to the name of the raised
   -- exception.
   --
   -- @return
   -- A POINTER to the ALLOCATED string.
   --
   -- Note: Free it!
   --
   function Get_Name return chars_ptr;
   pragma Export (C, Get_Name, "malef_getErrorName");

   --
   -- This function returns an ALLOCATED pointer to the message describing the
   -- last raised exception.
   --
   -- @return
   -- A POINTER to the ALLOCATED string.
   --
   -- Note: Free it!
   --
   function Get_Message return chars_ptr;
   pragma Export (C, Get_Message, "malef_getErrorMessage");

private

   -- The last exception.
   Last_Exception : Ada.Exceptions.Exception_Occurrence;

end C_Malef.Errors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
