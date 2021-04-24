-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - W R A P P E R . A D B                     --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                               G E N E R I C                               --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2021 José Antonio Verde Jiménez  All Rights Reserved  --
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

function Malef.Wrapper (Parameters : Parameters_Type)
                        return Return_Type is
   Return_Value : Return_Type;
begin

   if User_Function = null then
      raise Constraint_Error with
      "The function is null!";
   end if;

   Initialize;

   Return_Value := User_Function.all(Parameters => Parameters);

   Finalize;

   return Return_Value;

exception
   when Error : others =>
      if Has_Been_Initialized then
         Finalize;
      end if;
      -- We reraise the last exception and that's it.
      Ada.Exceptions.Reraise_Occurrence (Error);
end Malef.Wrapper;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
