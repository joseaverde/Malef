-------------------------------------------------------------------------------
--                                                                           --
--                    C _ M A L E F - C O L O R S . A D S                    --
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

with Malef.Colors;
with C_Malef.Surfaces; use C_Malef.Surfaces;

--
-- @summary
-- This package contains functions to work with colours and palettes.
--
-- @description
-- This package contains colours, palettes and some default palettes. You can
-- use this package to modify a Surface's colours. Read the original Ada
-- documentation or the C Header file. (I'm not going to write everything
-- three times).
--
package C_Malef.Colors is

   type Color_Kind is (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White)
      with Convention => C;
   for Color_Kind use
      (Black   => 0,
       Red     => 1,
       Green   => 2,
       Yellow  => 3,
       Blue    => 4,
       Magenta => 5,
       Cyan    => 6,
       White   => 7);

   type Palette_Type is array (bool'Range, Color_Kind'Range) of Color_Type
      with Convention => C;

   type Palette_Kind is (Malef_Palette,
                         VGA,
                         Windows_XP_Console,
                         Windows_PowerShell,
                         Visual_Studio_Code,
                         Windows_10_Console,
                         Terminal_App,
                         PuTTY,
                         mIRC,
                         xterm,
                         Ubuntu)
      with Convention => C;
   for Palette_Kind use
      (Malef_Palette      => 0,
       VGA                => 1,
       Windows_XP_Console => 2,
       Windows_PowerShell => 3,
       Visual_Studio_Code => 4,
       Windows_10_Console => 5,
       Terminal_App       => 6,
       PuTTY              => 7,
       mIRC               => 8,
       xterm              => 9,
       Ubuntu             => 10);


   -- TODO: Handle Constraint_Error
   function Get_Foreground (Surface :     Surface_Type;
                            Row     :     Row_Type;
                            Col     :     Col_Type;
                            Color   : out Color_Type)
                            return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_getSurfaceForeground";

   function Get_Background (Surface :     Surface_Type;
                            Row     :     Row_Type;
                            Col     :     Col_Type;
                            Color   : out Color_Type)
                            return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_getSurfaceBackground";

   function Set_Foreground (Surface  : Surface_Type;
                            From_Row : Row_Type;
                            To_Row   : Row_Type;
                            From_Col : Col_Type;
                            To_Col   : Col_Type;
                            Color    : Color_Type)
                            return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_setSurfaceForeground";

   function Set_Background (Surface  : Surface_Type;
                            From_Row : Row_Type;
                            To_Row   : Row_Type;
                            From_Col : Col_Type;
                            To_Col   : Col_Type;
                            Color    : Color_Type)
                            return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_setSurfaceBackground";


   function Get_Cursor_Foreground (Surface :     Surface_Type;
                                   Color   : out Color_Type)
                                   return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_getCursorForeground";

   function Get_Cursor_Background (Surface :     Surface_Type;
                                   Color   : out Color_Type)
                                   return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_getCursorBackground";

   function Set_Cursor_Foreground (Surface : Surface_Type;
                                   Color   : Color_Type)
                                   return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_setCursorForeground";

   function Set_Cursor_Background (Surface : Surface_Type;
                                   Color   : Color_Type)
                                   return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_setCursorBackground";


   function Get_Palette (Palette : out Palette_Type)
                         return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_getPalette";

   -- TODO: Check for ranges.
   function Get_Palette (Kind    :     Palette_Kind;
                         Palette : out Palette_Type)
                         return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_getPaletteKind";

   function Set_Palette (Palette : Palette_Type)
                         return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_setPalette";

   function Set_Palette (Kind : Palette_Kind)
                         return Error_Kind
      with Export        => True,
           Convention    => C,
           External_Name => "malef_setPaletteKind";

private

   -- The following functions are used to quickly converta Ada types to C
   -- types and viceversa.

   function To_Ada (Item : Color_Type)
                    return Malef.Color_Type
      with Pure_Function,
           Inline;

   function To_C (Item : Malef.Color_Type)
                  return Color_Type
      with Pure_Function,
           Inline;

   function To_Ada (Item : Palette_Type)
                    return Malef.Colors.Palette_Type
      with Pure_Function,
           Inline;

   function To_C (Item : Malef.Colors.Palette_Type)
                  return Palette_Type
      with Pure_Function,
           Inline;

   function To_Ada (Item : Palette_Kind)
                    return Malef.Colors.Palette_Kind
      with Pure_Function,
           Inline;

   function To_C (Item : Malef.Colors.Palette_Kind)
                  return Palette_Kind
      with Pure_Function,
           Inline;

end C_Malef.Colors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
