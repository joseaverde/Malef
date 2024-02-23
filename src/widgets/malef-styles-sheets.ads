-------------------------------------------------------------------------------
--                                                                           --
--               M A L E F - S T Y L E S - S H E E T S . A D S               --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   S P E C                              --
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

private with Ada.Containers.Vectors;

package Malef.Styles.Sheets with Preelaborate is

   type Style_Sheet is tagged private with
      Aggregate => (Empty     => Empty,
                    Add_Named => Insert);

   function Empty return Style_Sheet;

   procedure Insert (
      Sheet    : in out Style_Sheet;
      Key      : in     Wide_Wide_String;
      New_Item : in     Style);

private

   package Style_Vectors is
      new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Style);

   type Style_Sheet is tagged null record;

   function Empty return Style_Sheet is (
      Style_Sheet'(others => <>));

end Malef.Styles.Sheets;
