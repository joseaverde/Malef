-------------------------------------------------------------------------------
--                                                                           --
--                    C _ M A L E F - E R R O R S . A D B                    --
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

package body C_Malef.Errors is


   procedure Push (Ada_Exception : Ada.Exceptions.Exception_Occurrence) is
   begin

      Ada.Exceptions.Save_Occurrence(Target => Last_Exception,
                                     Source => Ada_Exception);

   end Push;


   procedure Pop is
   begin

      Ada.Exceptions.Save_Occurrence(Target => Last_Exception,
                                     Source => Ada.Exceptions.Null_Occurrence);

   end Pop;


   use type Ada.Exceptions.Exception_Id;
   function Flying_Exception return bool is
   begin

      return (if Ada.Exceptions.Exception_Identity(Last_Exception) =
                 Ada.Exceptions.Exception_Identity(Ada.Exceptions.
                                                   Null_Occurrence)
               then False
               else True);

   end Flying_Exception;


   function Get_Name return chars_ptr is
   begin

      -- TODO: Remind user to free exception message.
      return New_String (Ada.Exceptions.Exception_Name(Last_Exception));

   end Get_Name;


   function Get_Message return chars_ptr is
   begin

      -- TODO: Remind user to free exception message.
      return New_String (Ada.Exceptions.Exception_Message(Last_Exception));

   end Get_Message;

begin

   Ada.Exceptions.Save_Occurrence(Target => Last_Exception,
                                  Source => Ada.Exceptions.Null_Occurrence);

end C_Malef.Errors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
