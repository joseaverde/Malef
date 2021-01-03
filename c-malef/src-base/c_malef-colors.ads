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
--
--
-- @description
--
package C_Malef.Colors is

   type Color_Kind is (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White);
   for Color_Kind use
      (Black   => 0,
       Red     => 1,
       Green   => 2,
       Yellow  => 3,
       Blue    => 4,
       Magenta => 5,
       Cyan    => 6,
       White   => 7);
   pragma Convention (C, Color_Kind);

   type Palette_Type is array (bool'Range, Color_Kind'Range) of Color_Type;
   pragma Convention (C, Palette_Type);

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
                         Ubuntu);
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
   pragma Convention (C, Palette_Kind);

   procedure Get_Foreground (Surface :     Surface_Type;
                             Row     :     Row_Type;
                             Col     :     Col_Type;
                             Color   : out Color_Type);
   pragma Export (C, Get_Foreground, "malef_getSurfaceForeground");


   procedure Get_Background (Surface :     Surface_Type;
                             Row     :     Row_Type;
                             Col     :     Col_Type;
                             Color   : out Color_Type);
   pragma Export (C, Get_Background, "malef_getSurfaceBackground");

   procedure Set_Foreground (Surface  : Surface_Type;
                             From_Row : Row_Type;
                             To_Row   : Row_Type;
                             From_Col : Col_Type;
                             To_Col   : Col_Type;
                             Color    : Color_Type);
   pragma Export (C, Set_Foreground, "malef_setSurfaceForeground");

   procedure Set_Background (Surface  : Surface_Type;
                             From_Row : Row_Type;
                             To_Row   : Row_Type;
                             From_Col : Col_Type;
                             To_Col   : Col_Type;
                             Color    : Color_Type);
   pragma Export (C, Set_Background, "malef_setSurfaceBackground");


   procedure Get_Cursor_Foreground (Surface :     Surface_Type;
                                    Color   : out Color_Type);
   pragma Export (C, Get_Cursor_Foreground, "malef_getCursorForeground");

   procedure Get_Cursor_Background (Surface :     Surface_Type;
                                    Color   : out Color_Type);
   pragma Export (C, Get_Cursor_Background, "malef_getCursorBackground");

   procedure Set_Cursor_Foreground (Surface : Surface_Type;
                                    Color   : Color_Type);
   pragma Export (C, Set_Cursor_Foreground, "malef_setCursorForeground");

   procedure Set_Cursor_Background (Surface : Surface_Type;
                                    Color   : Color_Type);
   pragma Export (C, Set_Cursor_Background, "malef_setCursorBackground");


   procedure Get_Palette (Palette : out Palette_Type);
   pragma Export (C, Get_Palette, "malef_getPalette");

   procedure Get_Palette (Kind    :     Palette_Kind;
                          Palette : out Palette_Type);
   pragma Export (C, Get_Palette, "malef_getPaletteKind");

   procedure Set_Palette (Palette : Palette_Type);
   pragma Export (C, Set_Palette, "malef_setPalette");

   procedure Set_Palette (Kind : Palette_Kind);
   pragma Export (C, Set_Palette, "malef_setPaletteKind");

private

   -- TODO: Check if it's truly worth to inline it.
   function To_Ada (Item : Color_Type)
                    return Malef.Color_Type;
   pragma Pure_Function (To_Ada);
   pragma Inline (To_Ada);

   function To_C (Item : Malef.Color_Type)
                  return Color_Type;
   pragma Pure_Function (To_C);
   pragma Inline (To_C);

   function To_Ada (Item : Palette_Type)
                    return Malef.Colors.Palette_Type;
   pragma Pure_Function (To_Ada);
   pragma Inline (To_Ada);

   function To_C (Item : Malef.Colors.Palette_Type)
                  return Palette_Type;
   pragma Pure_Function (To_C);
   pragma Inline (To_C);

   function To_Ada (Item : Palette_Kind)
                    return Malef.Colors.Palette_Kind;
   pragma Pure_Function (To_Ada);
   pragma Inline (To_Ada);

   function To_C (Item : Malef.Colors.Palette_Kind)
                  return Palette_Kind;
   pragma Pure_Function (To_C);
   pragma Inline (To_C);


end C_Malef.Colors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
