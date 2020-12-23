-------------------------------------------------------------------------------
--                                                                           --
--                    M A L E F - S U R F A C E S . A D S                    --
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

private with Ada.Finalization;

--
-- @summary
-- This child package defines the Surface_Type and its primitive operations.
--
-- @description
-- The Surface_Type is a Controlled type which means it is able to clean itself
-- when a scope finishes.
--
package Malef.Surfaces is

   type Surface_Type is private;

--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
private --*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--
--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-

   --
   -- This is the Surface_Type: a Controlled type containing a Shared_Surface
   -- access type.
   --
   -- @field Reference
   -- The surface the current controlled type is pointing at.
   --
   type Surface_Type is new Ada.Finalization.Controlled with
      record
         Reference :  not null Shared_Surface_Access
                   := Shared_Null_Surface'Access;
      end record;

   overriding procedure Initialize (Object : in out Surface_Type);
   overriding procedure Adjust     (Object : in out Surface_Type);
   overriding procedure Finalize   (Object : in out Surface_Type);

   procedure Reference   (Item : not null Shared_Surface_Access);
   procedure Unreference (Item : not null Shared_Surface_Access);
                     
end Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
