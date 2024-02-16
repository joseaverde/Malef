-------------------------------------------------------------------------------
--                                                                           --
--             M A L E F - P L A T F O R M - I M A G E S . A D B             --
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

package body Malef.Platform.Images is

   Images : constant array (0 .. 9) of Digit := "0123456789";

   function Image (
      Row : in Row_Type)
      return String
   is
      Item : Row_Type := Row;
   begin
      return Result : String (1 .. 5) do
         for I in reverse Result'Range loop
            Result (I) := Images (Natural ((Item mod 10)));
            Item := Item / 10;
         end loop;
      end return;
   end Image;

   function Image (
      Col : in Col_Type)
      return String
   is
      Item : Col_Type := Col;
   begin
      return Result : String (1 .. 5) do
         for I in reverse Result'Range loop
            Result (I) := Images (Natural ((Item mod 10)));
            Item := Item / 10;
         end loop;
      end return;
   end Image;

   function Image (
      Component : in Component_Type)
      return String is
      ([Images (Integer (Component  / 100))
      , Images (Integer ((Component / 10) mod 10))
      , Images (Integer ((Component mod 10)))]);

end Malef.Platform.Images;
