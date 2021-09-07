-------------------------------------------------------------------------------
--                                                                           --
--                         M A L E F - S D K . A D S                         --
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

with Malef.Boxes;
with Malef.Surfaces;

--
-- @summary
-- This package is meant to facilitate the creation of interactive porgrams
-- with the Malef API.
--
-- @description
-- This package declares a new type called Widget which is derived from the Box
-- type which helps with the creation of interactive applications such as
-- message boxes or other kind of things. This was planned to be added in
-- future versions but I needed it for a current project.
--
package Malef.SDK is

   Default_Shadow_Color : constant Color_Type := (0, 0, 0, 128);

   generic
      type Return_Type is (<>);
   package Widgets is

      type Widget_Type is abstract new Base_Type with private;

      function Run (Widget : in out Widget_Type)
         return Return_Type is abstract;

   private

      type Widget_Type is abstract new Base_Type with
         record
            Box     : Malef.Boxes.Box_Type;
            Surface : Malef.Surfaces.Surface_Type;
            Shadow  : Malef.Surfaces.Surface_Type;
         end record;

      overriding
      function Get_Reference (Widget : in Widget_Type)
         return Surface_Reference;

   end Widgets;

end Malef.SDK;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
