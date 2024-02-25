-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - S T Y L E S . A D S                      --
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

package Malef.Styles with Preelaborate is

   -- This type provides a simple way of specifying the style for your
   -- application using Ada or a CSS-like language.

   --<<------------>>--
   -->> Style Type <<--
   --<<------------>>--

   type Style_Field is (

      -->> General <<--

      Background, Foreground,

      -->> Borders <<--

      Border_Foreground, Border_Background, Border_Style, Border_Thickness,

      -->> Text <<--

      Text_Background, Text_Foreground);

   type Style is private with
      Aggregate => (Empty     => Empty,
                    Add_Named => Set);

   function Empty return Style;

   procedure Set (
      Object : in out Style;
      Field  : in     Style_Field;
      Value  : in     Glyph_String);

   -- procedure Load (Stream : not null access Root_Stream_Type'Class);
   -- procedure Load (File : in String);
   -- procedure Dump (Stream : not null access Root_Stream_Type'Class);
   -- procedure Dump (File : in String);
   -- function "+" (Left, Right : in Style);
   -- Subpackage Malef.Styles.Watcher that updates a style if the file has
   -- changed.
   --
   -- Example usage:
   --
   --    [Text_Color     => "#F02312",
   --     Text_Alignment => Align_Left];
   --
   -- .css
   --
   --    Button {
   --       text-color     : #F02312;
   --       text-alignment : left-align;
   --    }

private

   --<<------------>>--
   -->> Style Type <<--
   --<<------------>>--

   type Style is
      record
         X : Integer;
      end record;

   function Empty return Style is (
      Style'(X => 5));

end Malef.Styles;
