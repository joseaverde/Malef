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
-- This package declares the functions to work with colours and palettes.
--
-- @description
-- This package contains the necessary functions to modify a Surface and its
-- colours in a simple way. It also contains the Palette type, which will be
-- used by the Malef.Systems package to generate colours accordingly to the
-- target terminal/console. It can also be used to store colours.
--
package Malef.Colors is

   --
   -- These are the names of the colours a palette can hold. (I have to
   -- describe each of the colours so `gnatdoc' doesn't warn me to document
   -- them, so here you are: Useless Information directly from Wikipedia).
   --
   -- @value Black
   -- Black isn't actually a colour, Black is the absence or complete
   -- absorption of visible light.
   --
   -- @value Red
   -- Red is the colour at the end of the visible spectrum of light.
   --
   -- @value Green
   -- Green is the colour between blue and yellow on the visible spectrum.
   --
   -- @value Yellow
   -- Yellow is the colour between orange and green on the spectrum of visible
   -- light.
   --
   -- @value Blue
   -- Blue is one of the three primary colours in pigments and traditional
   -- colour theory, as well as in the RGB colour model. It lies between violet
   -- and green on the spectrum of visible light.
   --
   -- @value Magenta
   -- Magenta is a colour that is variously defined as purplish-red, reddish-
   -- purple or mauvish-crimsom. On colour wheels of the RGB (additive) and CMY
   -- (subtractive) colour models, it is located midway between red and blye;
   -- this makes magenta an "imposible colour".
   --
   -- @value Cyan
   -- Cyan is a greenish-blue colour. It is evoked by light with a predominant
   -- wavelength of between 490 and 520 nm, between the wavelengths of green
   -- and blue.
   --
   -- @value White
   -- White is the lightest colour and is achromatic (having no hue).
   --
   type Color_Kind is (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White);

   --
   -- This is the palette type, the first index value (the boolean one)
   -- represents the brightness; the sencond one is name it has (it may not be
   -- the exact same colour).
   --
   type Palette_Type is array (Boolean'Range, Color_Kind'Range) of Color_Type;

   --
   -- These are the kind of palettes that are defaut for certain systems, it's
   -- only purpose is to use names instead of numbers to store the palettes.
   --
   -- @value Malef_Palette
   -- This is the default palette for this package (when the package is
   -- initialized other palettes are selected). This palette is used to give
   -- Malef a new feel.
   --
   -- @value VGA
   -- Typical colours that are used when booting PCs and leaving them in text
   -- mode, which used a 16-entry colour table. The colours are different in
   -- the EGA/VGA graphic modes.
   --
   -- @value Windows_XP_Console
   -- The Windows Console is the infrastructure for consoe applications in
   -- Microsof Windows.
   --
   -- @value Windows_PowerShell
   -- PowerShell is a task automation and configuration management framework
   -- from Microsoft, consisting of a command-line shell and the associated
   -- scripting-language.
   --
   -- @value Visual_Studio_Code
   -- Visual_Studio_Code is a free source-code editor made for Windows,
   -- Linux and macOS, these are the colours for its debug console.
   --
   -- @value Windows_10_Console
   -- These are the colours for the Windows 10 Console and PowerShell 6, (see
   -- the values above for more information about what is each of them).
   --
   -- @value Terminal_App
   -- Terminal.app is the terminal emulator included in the macOS operating
   -- system by Apple.
   --
   -- @value PuTTY
   -- PuTTY is a free and open-source terminal emulator, serial console and
   -- network file transfer application.
   --
   -- @value mIRC
   -- mIRC is and Internet Relay Chat (IRC) client for Windows, created in 1995
   --
   -- @value xterm
   -- In computing, xterm is the standard emulator for the X Window system.
   --
   -- @value Ubuntu
   -- Ubuntu is a Linux distribution based on Debian and mostly composed of
   -- free and open-source software.
   --
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

   -- These are the default palettes for several systems.
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
                                        Red     => (255,   0,   0, 255),
                                        Green   => (  0, 255,   0, 255),
                                        Yellow  => (255, 255,   0, 255),
                                        Blue    => (  0,   0, 255, 255),
                                        Magenta => (255,   0, 255, 255),
                                        Cyan    => (  0, 255, 255, 255),
                                        White   => (255, 255, 255, 255))),

       Windows_PowerShell => (False => (Black   => (  0,   0,   0, 255),
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

       PuTTY => (False => (Black   => (  0,   0,   0,   0),
                           Red     => (187,   0,   0, 255),
                           Green   => (  0, 187,   0, 255),
                           Yellow  => (187, 187,   0, 255),
                           Blue    => (  0,   0, 187, 255),
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
                            Red     => (255,   0,   0, 255),
                            Green   => (  0, 255,   0, 255),
                            Yellow  => (255, 255,   0, 255),
                            Blue    => (  0,   0, 255, 255),
                            Magenta => (255,   0, 255, 255),
                            Cyan    => (  0, 255, 255, 255),
                            White   => (255, 255, 255, 255))));


   Transparent : constant Color_Type := (  0,   0,   0,   0);


   --============-----------------------============--
   --============-- COLOUR OPERATIONS --============--
   --============-----------------------============--
   --
   -- These are the operations (functions and procedures) declared in this
   -- package to get or set the foreground/background colour in a certain
   -- position or block of the Surface.
   --

   --====----------------====--
   --====-- GET COLOUR --====--
   --====----------------====--

   --
   -- This procedure retrieves the foreground colour of a given position.
   --
   -- @param Surface
   -- The surface from which the information will be retrieved.
   --
   -- @param Row
   -- The row from which the foreground colour will be retrieved.
   --
   -- @param Col
   -- The column from which the foreground colour will be retrieved.
   --
   -- @param Color
   -- The retrieved colour.
   --
   -- @exception Malef.Exceptions.Bounds_Error
   -- This exception is raised when the Row or the Column are out of the
   -- Surfaces's bounds.
   --
   procedure Get_Foreground (Surface :     Malef.Surfaces.Surface_Type;
                             Row     :     Row_Type;
                             Col     :     Col_Type;
                             Color   : out Color_Type);

   --
   -- This procedure retrieves the background colour of a given position.
   --
   -- @param Surface
   -- The surface from which the information will be retrieved.
   --
   -- @param Row
   -- The row from which the background colour will be retrieved.
   --
   -- @param Col
   -- The column from which the background colour will be retrieved.
   --
   -- @param Color
   -- The retrieved colour.
   --
   -- @exception Malef.Exceptions.Bounds_Error
   -- This exception is raised when the Row or the Column are out of the
   -- Surfaces's bounds.
   --
   procedure Get_Background (Surface :     Malef.Surfaces.Surface_Type;
                             Row     :     Row_Type;
                             Col     :     Col_Type;
                             Color   : out Color_Type);

   --
   -- This function retrieves the foreground colour of a given position.
   --
   -- @param Surface
   -- The surface from which the information will be retrieved.
   --
   -- @param Row
   -- The row from which the foreground colour will be retrieved.
   --
   -- @param Col
   -- The column from which the foreground colour will be retrieved.
   --
   -- @return Color
   -- The retrieved colour.
   --
   -- @exception Malef.Exceptions.Bounds_Error
   -- This exception is raised when the Row or the Column are out of the
   -- Surfaces's bounds.
   --
   function Get_Foreground (Surface : Malef.Surfaces.Surface_Type;
                            Row     : Row_Type;
                            Col     : Col_Type)
                            return Color_Type;
   --
   -- This function retrieves the background colour of a given position.
   --
   -- @param Surface
   -- The surface from which the information will be retrieved.
   --
   -- @param Row
   -- The row from which the background colour will be retrieved.
   --
   -- @param Col
   -- The column from which the background colour will be retrieved.
   --
   -- @return Color
   -- The retrieved colour.
   --
   -- @exception Malef.Exceptions.Bounds_Error
   -- This exception is raised when the Row or the Column are out of the
   -- Surfaces's bounds.
   --
   function Get_Background (Surface : Malef.Surfaces.Surface_Type;
                            Row     : Row_Type;
                            Col     : Col_Type)
                            return Color_Type;

   --====----------------====--
   --====-- SET COLOUR --====--
   --====----------------====--

   --
   -- This procedure changes the foreground colour in a certain position of the
   -- Surface.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Row
   -- The row to edit.
   --
   -- @param Col
   -- The column to edit.
   --
   -- @param Color
   -- The colour to set in the given position.
   --
   -- @exception Malef.Exceptions.Null_Surface_Error
   -- This exceptions is raised if you are trying to edit THE NULL SURFACE.
   --
   -- @exception Malef.Exceptions.Bounds_Error
   -- This exceptions is raised if trying to change the colour of a position
   -- out of the given Surface's bounds.
   --
   procedure Set_Foreground (Surface : Malef.Surfaces.Surface_Type;
                             Row     : Row_Type;
                             Col     : Col_Type;
                             Color   : Color_Type);

   --
   -- This procedure changes the background colour in a certain position of the
   -- Surface.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Row
   -- The row to edit.
   --
   -- @param Col
   -- The column to edit.
   --
   -- @param Color
   -- The colour to set in the given position.
   --
   -- @exception Malef.Exceptions.Null_Surface_Error
   -- This exceptions is raised if you are trying to edit THE NULL SURFACE.
   --
   -- @exception Malef.Exceptions.Bounds_Error
   -- This exceptions is raised if trying to change the colour of a position
   -- out of the given Surface's bounds.
   --
   procedure Set_Background (Surface : Malef.Surfaces.Surface_Type;
                             Row     : Row_Type;
                             Col     : Col_Type;
                             Color   : Color_Type);

   --
   -- This procedure changes the foreground colour in a block of the Surface,
   -- i.e. you can specify the ranges to change.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param From_Row
   -- The row to start editing.
   --
   -- @param To_Row
   -- The row to finish editing.
   --
   -- @param From_Col
   -- The column to start editing.
   --
   -- @param To_Col
   -- The column to finish editing.
   --
   -- @param Color
   -- The colour to set in the given block.
   --
   -- @exception Malef.Exceptions.Null_Surface_Error
   -- This exceptions is raised if you are trying to edit THE NULL SURFACE.
   --
   -- @exception Malef.Exceptions.Bounds_Error
   -- This exceptions is raised if trying to change the colour in any position
   -- out of the Surfaces's ranges. It's also raised if any or both of the `TO'
   -- parameters is lower than the `FROM' one.
   --
   procedure Set_Foreground (Surface  : Malef.Surfaces.Surface_Type;
                             From_Row : Row_Type;
                             To_Row   : Row_Type;
                             From_Col : Col_Type;
                             To_Col   : Col_Type;
                             Color    : Color_Type);

   --
   -- This procedure changes the background colour in a block of the Surface,
   -- i.e. you can specify the ranges to change.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param From_Row
   -- The row to start editing.
   --
   -- @param To_Row
   -- The row to finish editing.
   --
   -- @param From_Col
   -- The column to start editing.
   --
   -- @param To_Col
   -- The column to finish editing.
   --
   -- @param Color
   -- The colour to set in the given block.
   --
   -- @exception Malef.Exceptions.Null_Surface_Error
   -- This exceptions is raised if you are trying to edit THE NULL SURFACE.
   --
   -- @exception Malef.Exceptions.Bounds_Error
   -- This exceptions is raised if trying to change the colour in any position
   -- out of the Surfaces's ranges. It's also raised if any or both of the `TO'
   -- parameters is lower than the `FROM' one.
   --
   procedure Set_Background (Surface  : Malef.Surfaces.Surface_Type;
                             From_Row : Row_Type;
                             To_Row   : Row_Type;
                             From_Col : Col_Type;
                             To_Col   : Col_Type;
                             Color    : Color_Type);

   --
   -- This procedure changes the foreground colour on all the surface.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Color
   -- The colour to set.
   --
   -- @exception Malef.Exceptions.Null_Surface_Error
   -- This exceptions is raised if you are trying to edit THE NULL SURFACE.
   --
   procedure Set_Foreground (Surface : Malef.Surfaces.Surface_Type;
                             Color   : Color_Type);

   --
   -- This procedure changes the background colour on all the surface.
   --
   -- @param Surface
   -- The surface to edit.
   --
   -- @param Color
   -- The colour to set.
   --
   -- @exception Malef.Exceptions.Null_Surface_Error
   -- This exceptions is raised if you are trying to edit THE NULL SURFACE.
   --
   procedure Set_Background (Surface : Malef.Surfaces.Surface_Type;
                             Color   : Color_Type);



   --============------------------------------============--
   --============-- CURSOR COLOUR OPERATIONS --============--
   --============------------------------------============--
   --
   -- The cursor colour is the default colour the cursor writes new text, this
   -- can be changed in this child package with the following functions.
   --

   --====----------------====--
   --====-- GET COLOUR --====--
   --====----------------====--

   --
   -- This procedure gets the default cursor writing foreground colour.
   --
   -- @param Surface
   -- The surface whose default cursor writing colour we want to change.
   --
   -- @param Color
   -- The retrieved colour.
   --
   procedure Get_Cursor_Foreground (Surface :     Malef.Surfaces.Surface_Type;
                                    Color   : out Color_Type);

   --
   -- This procedure gets the default cursor writing background colour.
   --
   -- @param Surface
   -- The surface whose default cursor writing colour we want to change.
   --
   -- @param Color
   -- The retrieved colour.
   --
   procedure Get_Cursor_Background (Surface :     Malef.Surfaces.Surface_Type;
                                    Color   : out Color_Type);

   --
   -- This function gets the default cursor writing foreground colour.
   --
   -- @param Surface
   -- The surface whose default cursor writing colour we want to retrieve.
   --
   -- @return Color
   -- The retrieved colour.
   --
   function Get_Cursor_Foreground (Surface : Malef.Surfaces.Surface_Type)
                                   return Color_Type;

   --
   -- This function gets the default cursor writing background colour.
   --
   -- @param Surface
   -- The surface whose default cursor writing colour we want to retrieve.
   --
   -- @return Color
   -- The retrieved colour.
   --
   function Get_Cursor_Background (Surface : Malef.Surfaces.Surface_Type)
                                   return Color_Type;

   --====----------------====--
   --====-- SET COLOUR --====--
   --====----------------====--

   --
   -- This procedure changes the default writing foreground colour of the
   -- Surfaces' cursor.
   --
   -- @param Surface
   -- The Surface whose default cursor writing colour we want to change.
   --
   -- @param Color
   -- The colour to set.
   --
   procedure Set_Cursor_Foreground (Surface : Malef.Surfaces.Surface_Type;
                                    Color   : Color_Type);

   --
   -- This procedure changes the default writing background colour of the
   -- Surfaces' cursor.
   --
   -- @param Surface
   -- The Surface whose default cursor writing colour we want to change.
   --
   -- @param Color
   -- The colour to set.
   --
   procedure Set_Cursor_Background (Surface : Malef.Surfaces.Surface_Type;
                                    Color   : Color_Type);


   --============-----------------------============--
   --============-- COLOUR OPERATIONS --============--
   --============-----------------------============--


   --====-----------------====--
   --====-- GET PALETTE --====--
   --====-----------------====--

   --
   -- This function gets the palette now in use.
   --
   -- @param Palette
   -- The retrieved palette.
   --
   -- @exception Malef.Exceptions.Initialization_Error
   -- This exception is raised if the library hasn't been initialized yet.
   --
   procedure Get_Palette (Palette : out Palette_Type);

   --
   -- This function gets the palette now in use.
   --
   -- @return
   -- The retrieved palette.
   --
   function Get_Palette return Palette_Type;


   --====-----------------====--
   --====-- SET PALETTE --====--
   --====-----------------====--

   --
   -- This procedure changes the palette in use for another one.
   --
   -- @param Palette
   -- The palette to set.
   --
   procedure Set_Palette (Palette : in Palette_Type);

   --
   -- This procedure sets a new palette, but it takes a `Palette_Kind' variable
   -- as parameter to avoid writing: Malef.Color.Palettes(Malef.Color.<>)
   -- in every package.
   --
   -- @param Kind
   -- The palette to set.
   --
   procedure Set_Palette (Kind : Palette_Kind);


private

   -- This is the current palette in use, it can't be changed before
   -- initialisation because it may be changed after initialization.
   Current_Palette : Palette_Type := Palettes(Malef_Palette);

end Malef.Colors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
