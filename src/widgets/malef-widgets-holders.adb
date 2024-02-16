-------------------------------------------------------------------------------
--                                                                           --
--             M A L E F - W I D G E T S - H O L D E R S . A D B             --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   B O D Y                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2021-2024 José Antonio Verde Jiménez  All Rights Reserved  --
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

with Malef.Empty;

package body Malef.Widgets.Holders  is

   procedure Hold (
      Object : in out Holder;
      Item   : in     Widget'Class) is
   begin
      Object.Item.Replace_Element (Item);
   end Hold;

   procedure Release (
      Object : in out Holder)
   is
      Empty : Malef.Empty.Empty_Widget;
   begin
      Object.Item.Replace_Element (Empty);
   end Release;

   overriding
   procedure Initialize (Object : in out Holder) is
   begin
      Object.Release;
   end Initialize;

end Malef.Widgets.Holders;
