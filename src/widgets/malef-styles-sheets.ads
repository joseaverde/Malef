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

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Wide_Wide_Hash;

package Malef.Styles.Sheets with Preelaborate is

   type Style_Sheet is tagged private with
      Put_Image => Put_Image,
      Aggregate => (Empty     => Empty,
                    Add_Named => Insert);

   function Empty return Style_Sheet;

   procedure Insert (
      Sheet    : in out Style_Sheet;
      Key      : in     Wide_Wide_String;
      New_Item : in     Style);

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Style_Sheet);

private

   package Style_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps (
      Key_Type        => Wide_Wide_String,
      Element_Type    => Style,
      Hash            => Ada.Strings.Wide_Wide_Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   type Style_Sheet is tagged
      record
         Map : Style_Maps.Map;
      end record;

   function Empty return Style_Sheet is (
      Style_Sheet'(others => <>));

end Malef.Styles.Sheets;
