-------------------------------------------------------------------------------
--                                                                           --
--                         M A L E F - S D K . A D B                         --
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

--with Ada.Unchecked_Deallocation;

package body Malef.SDK is

   package body Widgets is

      overriding
      function Get_Reference (Widget : in Widget_Type)
         return Surface_Reference is
      begin

         return Widget.Box.Get_Reference;

      end Get_Reference;


      procedure Assign (Widget : in out Widget_Type;
         Position : Return_Type;
         Element  : Any_Widget_Access) is
      begin

         Widget.Next(Position) := Element;

      end Assign;

   end Widgets;

end Malef.SDK;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
