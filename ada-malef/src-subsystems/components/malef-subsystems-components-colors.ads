-------------------------------------------------------------------------------
--                                                                           --
--   MALEF - S U B S Y S T E M S - C O M P O N E N T S - C O L O R S . ADS   --
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

with Malef.Colors; use Malef.Colors;

-- TODO: Comment it
-- @summary
--
--
-- @description
--
package Malef.Subsystems.Components.Colors is

   type H_Type  is delta 0.1 digits 4 range 0.0 .. 255.9;
   type SB_Type is           digits 6 range 0.0 .. 1.0;

   type HSB_Type is
      record
         Hue        : H_Type;
         Saturation : SB_Type;
         Brightness : SB_Type;
      end record;

   procedure Precalculate;

   procedure To_Color_1 (Color : in HSB_Type;
      Is_White : out Boolean)
      with Inline;

   procedure To_Color_3 (Color : in HSB_Type;
      Out_Color : out Color_Kind)
      with Inline;

   procedure To_Color_4 (Color : HSB_Type;
      Out_Color  : out Color_Kind;
      Brightness : out Boolean)
      with Inline;

   procedure To_Color_8 (Color : Color_Type;
      Out_Color : out Color_Component_Type)
      with Inline;

   function To_HSB (Color : Color_Type)
      return HSB_Type
      with Pure_Function;

   function To_RGB (Color : HSB_Type)
      return Color_Type
      with Pure_Function;

private

   Current_Palette : Palette_Type;
   The_Palette : array(Palette_Type'Range(1),Palette_Type'Range(2))of HSB_Type;

   type Percentage is delta 0.0001 range 0.0000 .. 1.0000;

   function Color_Match (Left, Right : HSB_Type)
      return Percentage
      with Inline, Pure_Function;

end Malef.Subsystems.Components.Colors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================--- 
