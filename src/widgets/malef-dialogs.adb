-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - D I A L O G S . A D B                     --
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

with Malef.Palettes;

package body Malef.Dialogs is

   procedure Get_Size (
      Object : in     Dialog;
      Rows   :    out Positive_Row_Count;
      Cols   :    out Positive_Col_Count) is
   begin
      Rows := Object.Rows;
      Cols := Object.Cols;
   end Get_Size;

   Roll_Symbol     : constant Glyph := '^';
   Minimize_Symbol : constant Glyph := '-';
   Maximize_Symbol : constant Glyph := '□';
   Close_Symbol    : constant Glyph := 'x';

   procedure Draw (
      Object  : in     Dialog;
      Surface : in out Surfaces.Surface)
   is
      use Malef.Palettes;
   begin
      Surface.Fill (' ');
      Surface.Fill_Background (Default_Dialog_Background);

      -- Corners

      Surface (2,            1)            := '┌';
      Surface (2,            Surface.Cols) := '┐';
      Surface (Surface.Rows, 1)            := '└';
      Surface (Surface.Rows, Surface.Cols) := '┘';

      -- Rows

      Surface.Fill ((2, 2), (2, Surface.Cols - 1), '─');
      Surface.Fill_Foreground ((2, 1), (2, Surface.Cols - 1),
                               Default_Dialog_Light_Foreground);
      Surface.Fill ((Surface.Rows, 2), (Surface.Rows, Surface.Cols - 1), '─');
      Surface.Fill_Foreground ((Surface.Rows, 2), (Surface.Rows, Surface.Cols),
                               Default_Dialog_Dark_Foreground);

      -- Columns

      Surface.Fill ((3, 1), (Surface.Rows - 1, 1), '│');
      Surface.Fill_Foreground ((3, 1), (Surface.Rows - 1, 1),
                               Default_Dialog_Light_Foreground);
      Surface.Fill ((3, Surface.Cols), (Surface.Rows - 1, Surface.Cols), '│');
      Surface.Fill_Foreground ((3, Surface.Cols),
                               (Surface.Rows - 1, Surface.Cols),
                               Default_Dialog_Dark_Foreground);

      -- Text and Buttonqs

      Surface.Put (1, 2, To_Wide_Wide_String (Object.Title));
      Surface.Fill_Background ((1, Surface.Cols - 3),
                               (1, Surface.Cols - 1), Malef.Palettes.Red);
      Surface.Fill_Background ((1, Surface.Cols - 9),
                               (1, Surface.Cols - 4),
                               Malef.Palettes.Light_White);
      Surface.Set (1, Surface.Cols - 2, Close_Symbol);
      Surface.Set (1, Surface.Cols - 5, Maximize_Symbol);
      Surface.Set (1, Surface.Cols - 8, Minimize_Symbol);
      Surface.Set (1, Surface.Cols - 11, Roll_Symbol);

      Object.Widget.Constant_Reference.On_Draw (Surface,
         ((3, 2), (Surface.Rows - 1, Surface.Cols - 1)));

   end Draw;

end Malef.Dialogs;
