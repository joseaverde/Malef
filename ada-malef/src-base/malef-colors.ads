-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - C O L O R S . A D S                      --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2021 José Antonio Verde Jiménez  All Rights Reserved  --
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

limited with Malef.Surfaces;

--
-- @summary
--
--
-- @description
--
package Malef.Colors is

   type Color_Kind is (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White);
   type Palette_Type is array (Boolean'Range, Color_Kind'Range) of Color_Type;

   type Palette_Kind is (Malef_Palette,
                         VGA,
                         Windows_XP_Console,
                         Windows_Power_Shell,
                         Visual_Studio_Code,
                         Windows_10_Console,
                         Terminal_App,
                         PuTTY,
                         mIRC,
                         xterm,
                         Ubuntu);

   Palettes : constant array (Palette_Kind'Range) of Palette_Type :=
      (Malef_Palette => (False => (Black   => ( 31,  41,  59, 255),
                                   Red     => (192,  48,  96, 255),
                                   Green   => ( 48, 198,  96, 255),
                                   Yellow  => (192, 192,  48, 255),
                                   Blue    => ( 48,  96, 192, 255),
                                   Magenta => (192,  48, 192, 255),
                                   Cyan    => ( 48, 192, 192, 255),
                                   White   => (192, 192, 192, 255)),
                         True  => (Black   => ( 62,  82, 118, 255),
                                   Red     => (255,  64, 128, 255),
                                   Green   => ( 64, 255, 128, 255),
                                   Yellow  => (255, 255,  64, 255),
                                   Blue    => ( 64, 128, 255, 255),
                                   Magenta => (255,  64, 255, 255),
                                   Cyan    => ( 64, 255, 255, 255),
                                   White   => (255, 255, 255, 255))),

       VGA => (False => (Black   => (  0,   0,   0, 255),
                         Red     => (170,   0,   0, 255),
                         Green   => (  0, 170,   0, 255),
                         Yellow  => (170,  85,   0, 255),
                         Blue    => (  0,   0, 170, 255),
                         Magenta => (170,   0, 170, 255),
                         Cyan    => (  0, 170, 170, 255),
                         White   => (170, 170, 170, 255)),
               True  => (Black   => ( 85,  85,  85, 255),
                         Red     => (255,  85,  85, 255),
                         Green   => ( 85, 255,  85, 255),
                         Yellow  => (255, 255,  85, 255),
                         Blue    => ( 85,  85, 255, 255),
                         Magenta => (255,  85, 255, 255),
                         Cyan    => ( 85, 255, 255, 255),
                         White   => (255, 255, 255, 255))),

       Windows_XP_Console => (False => (Black   => (  0,   0,   0, 255),
                                        Red     => (128,   0,   0, 255),
                                        Green   => (  0, 128,   0, 255),
                                        Yellow  => (128, 128,   0, 255),
                                        Blue    => (  0,   0, 128, 255),
                                        Magenta => (128,   0, 128, 255),
                                        Cyan    => (  0, 128, 128, 255),
                                        White   => (192, 192, 192, 255)),
                              True  => (Black   => (128, 128, 128, 255),
                                        Red     => (255, 255, 255, 255),
                                        Green   => (  0, 255,   0, 255),
                                        Yellow  => (255, 255,   0, 255),
                                        Blue    => (  0,   0, 255, 255),
                                        Magenta => (255,   0, 255, 255),
                                        Cyan    => (  0, 255, 255, 255),
                                        White   => (255, 255, 255, 255))),

       Windows_Power_Shell => (False => (Black   => (  0,   0,   0, 255),
                                         Red     => (128,   0,   0, 255),
                                         Green   => (  0, 128,   0, 255),
                                         Yellow  => (238, 237, 240, 255),
                                         Blue    => (  0,   0, 128, 255),
                                         Magenta => (  1,  36,  86, 255),
                                         Cyan    => (  0, 128, 128, 255),
                                         White   => (192, 192, 192, 255)),
                               True  => (Black   => (128, 128, 128, 255),
                                         Red     => (255, 255, 255, 255),
                                         Green   => (  0, 255,   0, 255),
                                         Yellow  => (255, 255,   0, 255),
                                         Blue    => (  0,   0, 255, 255),
                                         Magenta => (255,   0, 255, 255),
                                         Cyan    => (  0, 255, 255, 255),
                                         White   => (255, 255, 255, 255))),

       Visual_Studio_Code => (False => (Black   => (  0,   0,   0, 255),
                                        Red     => (205,  49,  49, 255),
                                        Green   => ( 13, 188, 121, 255),
                                        Yellow  => (229, 229,  16, 255),
                                        Blue    => ( 36, 114, 200, 255),
                                        Magenta => (188,  63, 188, 255),
                                        Cyan    => ( 17, 168, 205, 255),
                                        White   => (229, 229, 229, 255)),
                              True  => (Black   => (102, 102, 102, 255),
                                        Red     => (241,  76,  76, 255),
                                        Green   => ( 35, 209, 139, 255),
                                        Yellow  => (245, 245,  67, 255),
                                        Blue    => ( 59, 142, 234, 255),
                                        Magenta => (214, 112, 214, 255),
                                        Cyan    => ( 41, 184, 219, 255),
                                        White   => (229, 229, 229, 255))),

       Windows_10_Console => (False => (Black   => ( 12,  12,  12, 255),
                                        Red     => (197,  15,  31, 255),
                                        Green   => ( 19, 161,  14, 255),
                                        Yellow  => (193, 156,   0, 255),
                                        Blue    => (  0,  55, 218, 255),
                                        Magenta => (136,  23, 152, 255),
                                        Cyan    => ( 58, 150, 221, 255),
                                        White   => (204, 204, 204, 255)),
                              True  => (Black   => (118, 118, 118, 255),
                                        Red     => (231,  72,  86, 255),
                                        Green   => ( 22, 198,  12, 255),
                                        Yellow  => (249, 241, 165, 255),
                                        Blue    => ( 59, 120, 255, 255),
                                        Magenta => (180,   0, 158, 255),
                                        Cyan    => ( 97, 214, 214, 255),
                                        White   => (242, 242, 242, 255))),

       Terminal_App => (False => (Black   => (  0,   0,   0, 255),
                                  Red     => (194,  54,  33, 255),
                                  Green   => ( 37, 188,  36, 255),
                                  Yellow  => (173, 173,  39, 255),
                                  Blue    => ( 73,  46, 255, 255),
                                  Magenta => (211,  56, 211, 255),
                                  Cyan    => ( 51, 187, 200, 255),
                                  White   => (203, 204, 205, 255)),
                        True  => (Black   => (129, 131, 131, 255),
                                  Red     => (252,  57,  31, 255),
                                  Green   => ( 49, 231,  34, 255),
                                  Yellow  => (234, 236,  35, 255),
                                  Blue    => ( 88,  51, 255, 255),
                                  Magenta => (249,  53, 248, 255),
                                  Cyan    => ( 20, 240, 240, 255),
                                  White   => (233, 235, 235, 255))),

       PuTTY => (False => (Black   => (187,   0,   0, 255),
                           Red     => (  0, 187,   0, 255),
                           Green   => (187, 187,   0, 255),
                           Yellow  => (  0,   0, 187, 255),
                           Blue    => (187,   0, 187, 255),
                           Magenta => (187,   0, 187, 255),
                           Cyan    => (  0, 187, 187, 255),
                           White   => (187, 187, 187, 255)),
                 True  => (Black   => ( 85,  85,  85, 255),
                           Red     => (255,  85,  85, 255),
                           Green   => ( 85, 255,  85, 255),
                           Yellow  => (255, 255,  85, 255),
                           Blue    => ( 85,  85, 255, 255),
                           Magenta => (255,  85, 255, 255),
                           Cyan    => ( 85, 255, 255, 255),
                           White   => (255, 255, 255, 255))),

       mIRC => (False => (Black   => (  0,   0,   0, 255),
                          Red     => (127,   0,   0, 255),
                          Green   => (  0, 147,   0, 255),
                          Yellow  => (252, 127,   0, 255),
                          Blue    => (  0,   0, 127, 255),
                          Magenta => (156,   0, 156, 255),
                          Cyan    => (  0, 147, 147, 255),
                          White   => (210, 210, 210, 255)),
                True  => (Black   => (127, 127, 127, 255),
                          Red     => (255,   0,   0, 255),
                          Green   => (  0, 252,   0, 255),
                          Yellow  => (255, 255,   0, 255),
                          Blue    => (  0,   0, 252, 255),
                          Magenta => (255,   0, 255, 255),
                          Cyan    => (  0, 255, 255, 255),
                          White   => (255, 255, 255, 255))),

       xterm => (False => (Black   => (  0,   0,   0, 255),
                           Red     => (205,   0,   0, 255),
                           Green   => (  0, 205,   0, 255),
                           Yellow  => (205, 205,   0, 255),
                           Blue    => (  0,   0, 238, 255),
                           Magenta => (205,   0, 205, 255),
                           Cyan    => (  0, 205, 205, 255),
                           White   => (229, 229, 229, 255)),
                 True  => (Black   => (127, 127, 127, 255),
                           Red     => (255,   0,   0, 255),
                           Green   => (  0, 255,   0, 255),
                           Yellow  => (255, 255,   0, 255),
                           Blue    => ( 92,  92, 255, 255),
                           Magenta => (255,   0, 255, 255),
                           Cyan    => (  0, 255, 255, 255),
                           White   => (255, 255, 255, 255))),

       Ubuntu => (False => (Black   => (  1,   1,   1, 255),
                            Red     => (222,  56,  43, 255),
                            Green   => ( 57, 181,  74, 255),
                            Yellow  => (255, 199,   6, 255),
                            Blue    => (  0, 111, 184, 255),
                            Magenta => (118,  38, 113, 255),
                            Cyan    => ( 44, 181, 233, 255),
                            White   => (204, 204, 204, 255)),
                  True  => (Black   => (128, 128, 128, 255),
                            Red     => (255, 255,   0, 255),
                            Green   => (  0, 255,   0, 255),
                            Yellow  => (255, 255,   0, 255),
                            Blue    => (  0,   0, 255, 255),
                            Magenta => (255,   0, 255, 255),
                            Cyan    => (  0, 255, 255, 255),
                            White   => (255, 255, 255, 255))));


   Transparent : constant Color_Type := (  0,   0,   0,   0);

   --
   -- This procedure changes
   procedure Get_Foreground (Surface :     Malef.Surfaces.Surface_Type;
                             Row     :     Row_Type;
                             Col     :     Col_Type;
                             Color   : out Color_Type);

   procedure Get_Background (Surface :     Malef.Surfaces.Surface_Type;
                             Row     :     Row_Type;
                             Col     :     Col_Type;
                             Color   : out Color_Type);

   function Get_Foreground (Surface : Malef.Surfaces.Surface_Type;
                            Row     : Row_Type;
                            Col     : Col_Type)
                            return Color_Type;

   function Get_Background (Surface : Malef.Surfaces.Surface_Type;
                            Row     : Row_Type;
                            Col     : Col_Type)
                            return Color_Type;


   procedure Set_Foreground (Surface : Malef.Surfaces.Surface_Type;
                             Row     : Row_Type;
                             Col     : Col_Type;
                             Color   : Color_Type);

   procedure Set_Background (Surface : Malef.Surfaces.Surface_Type;
                             Row     : Row_Type;
                             Col     : Col_Type;
                             Color   : Color_Type);

   procedure Set_Foreground (Surface  : Malef.Surfaces.Surface_Type;
                             From_Row : Row_Type;
                             To_Row   : Row_Type;
                             From_Col : Col_Type;
                             To_Col   : Col_Type;
                             Color    : Color_Type);

   procedure Set_Background (Surface  : Malef.Surfaces.Surface_Type;
                             From_Row : Row_Type;
                             To_Row   : Row_Type;
                             From_Col : Col_Type;
                             To_Col   : Col_Type;
                             Color    : Color_Type);

   procedure Set_Foreground (Surface : Malef.Surfaces.Surface_Type;
                             Color   : Color_Type);

   procedure Set_Background (Surface : Malef.Surfaces.Surface_Type;
                             Color   : Color_Type);



   procedure Get_Cursor_Foreground (Surface :     Malef.Surfaces.Surface_Type;
                                    Color   : out Color_Type);

   procedure Get_Cursor_Background (Surface :     Malef.Surfaces.Surface_Type;
                                    Color   : out Color_Type);

   function Get_Cursor_Foreground (Surface : Malef.Surfaces.Surface_Type)
                                   return Color_Type;

   function Get_Cursor_Background (Surface : Malef.Surfaces.Surface_Type)
                                   return Color_Type;


   procedure Set_Cursor_Foreground (Surface : Malef.Surfaces.Surface_Type;
                                    Color   : Color_Type);

   procedure Set_Cursor_Background (Surface : Malef.Surfaces.Surface_Type;
                                    Color   : Color_Type);



   procedure Set_Palette (Palette : in Palette_Type);

   procedure Set_Palette (Kind : Palette_Kind);
  

   procedure Get_Palette (Palette : out Palette_Type);
   
   function Get_Palette return Palette_Type;


private

   Current_Palette : Palette_Type := Palettes(Malef_Palette);

end Malef.Colors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
