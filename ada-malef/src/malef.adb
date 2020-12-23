-------------------------------------------------------------------------------
--                                                                           --
--                             M A L E F . A D B                             --
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
with Malef.Surfaces.Driver;

package body Malef is

   procedure Initialize is
   begin

      if Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library has already been initialized!";
      end if;

      Has_Been_Initialized := True;

      -- We create the null surface, which couldn't be used before
      -- initialization.

   -- Shared_Null_Surface.Grid(1, 1) := Element_Type'(
   --    Format => Format_Type'(Foreground_Color => (255, 255,   0, 255),
   --                           Background_Color => (255,   0,   0, 255),
   --                           Styles           => (others => False)),
   --    Char   => To_Char_Type('?'));

   end Initialize;


   procedure Finalize is
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library hasn't been initialized yet!";
      end if;

      Has_Been_Initialized := False;

   end Finalize;


   function Is_Initialized return Boolean is
   begin

      return Has_Been_Initialized;

   end Is_Initialized;




--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
--*--*- private -*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-



   function Get_Shared_Surface (Object : Malef.Surfaces.Surface_Type)
                                return Shared_Surface_Access is
   begin

      return Malef.Surfaces.Driver.Get_Reference (Object => Object);

   end Get_Shared_Surface;


end Malef;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
