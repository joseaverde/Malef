-------------------------------------------------------------------------------
--                                                                           --
--                    M A L E F - S U R F A C E S . A D B                    --
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

with Ada.Unchecked_Deallocation;
with System.Atomic_Counters;


package body Malef.Surfaces is


   overriding
   procedure Initialize (Object : in out Surface_Type) is
   begin

      Reference (Object.Reference);

   end Initialize;


   overriding
   procedure Adjust (Object : in out Surface_Type) is
   begin

      Reference (Object.Reference);

   end Adjust;

   
   overriding
   procedure Finalize (Object : in out Surface_Type) is
      Old_Reference :  constant not null Shared_Surface_Access
                    := Object.Reference;
   begin

      if Old_Reference /= Shared_Null_Surface'Access then
         -- This is used to avoid finalizing the same object twice.
         Object.Reference := Shared_Null_Surface'Access;
         Unreference(Old_Reference);
      end if;

   end Finalize;



   procedure Reference (Item : not null Shared_Surface_Access) is
   begin

      if Item = Shared_Null_Surface'Access then
         return;
      end if;

      System.Atomic_Counters.Increment (Item.Counter);

   end Reference;


   procedure Unreference (Item : not null Shared_Surface_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Shared_Surface_Type,
                                                        Shared_Surface_Access);
      Old : Shared_Surface_Access := Item;
   begin

      if Old = Shared_Null_Surface'Access then
         return;
      end if;

      if System.Atomic_Counters.Decrement (Old.Counter) then
         Free(Old);
      end if;

   end Unreference;

end Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
