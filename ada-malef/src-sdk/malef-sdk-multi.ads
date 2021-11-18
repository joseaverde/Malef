-------------------------------------------------------------------------------
--                                                                           --
--                   M A L E F - S D K - M U L T I . A D S                   --
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

private with Ada.Containers.Vectors;

--
-- @summary
-- This Widget it's used to make different Widgets execute at the same time.
--
-- @description
-- R
--
package Malef.SDK.Multi is

   type Return_Type is (None);
   package Multi_Widgets is new Widgets (Return_Type);
   type Multi_Widget_Type is
      new Multi_Widgets.Widget_Type with private;

   overriding
   function Run (Multi_Widget : in out Multi_Widget_Type)
      return Return_Type;

private

   package Widget_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Widgets.Any_Widget_Access
   );

   type Multi_Widget_Type is
      new Multi_Widget_Type with
      record
         Inside : Widget_Vectors.Vector;
      end record;

end Malef.SDK.Multi;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
