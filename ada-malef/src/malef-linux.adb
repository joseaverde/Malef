-------------------------------------------------------------------------------
--                                                                           --
--                       M A L E F - L I N U X . A D B                       --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
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

with Malef.Exceptions;

package body Malef.Linux is

   procedure Prepare_Terminal is
   begin
      null; -- TODO
   exception
      when others =>
         raise Malef.Exceptions.Initialization_Error;
   end Prepare_Terminal;


   procedure Restore_Terminal is
   begin
      null; -- TODO
   exception
      when others =>
         raise Malef.Exceptions.Initialization_Error;
   end Restore_Terminal;

end Malef.Linux;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
