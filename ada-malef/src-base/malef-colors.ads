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

   Black       : constant Color_Type := (  0,   0,   0, 255);
   Red         : constant Color_Type := (255,   0,   0, 255);
   Green       : constant Color_Type := (  0, 255,   0, 255);
   Blue        : constant Color_Type := (  0,   0, 255, 255);
   Yellow      : constant Color_Type := (255, 255,   0, 255);
   Magenta     : constant Color_Type := (255,   0, 255, 255);
   Cyan        : constant Color_Type := (  0, 255, 255, 255);
   White       : constant Color_Type := (255, 255, 255, 255);
   Transparent : constant Color_Type := (  0,   0,   0,   0);

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

end Malef.Colors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
