-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - D I A L O G S . A D S                     --
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

with Malef.Widgets;
with Malef.Surfaces;
private with Ada.Strings.Wide_Wide_Unbounded;
private with Malef.Widgets.Holders;

package Malef.Dialogs with Preelaborate is

   Default_Dialog_Height : constant := 40;
   Default_Dialog_Width  : constant := 12;

   type Dialog is tagged private;

   type Window_Mode is (Full_Screen, Maximized, Windowed);

   procedure Draw (
      Object  : in     Dialog;
      Surface : in out Surfaces.Surface);

   function New_Dialog (
      Widget : in Widgets.Widget'Class;
      Title  : in Glyph_String       := "";
      Mode   : in Window_Mode        := Windowed;
      Rows   : in Positive_Row_Count := Default_Dialog_Height;
      Cols   : in Positive_Col_Count := Default_Dialog_Width)
      return Dialog;

   function Get_Mode (
      Object : in Dialog)
      return Window_Mode;

   procedure Get_Size (
      Object : in     Dialog;
      Rows   :    out Positive_Row_Count;
      Cols   :    out Positive_Col_Count);

private

   use Ada.Strings.Wide_Wide_Unbounded;

   type Dialog is
      tagged record
         Widget : Widgets.Holders.Holder;
         Title  : Unbounded_Wide_Wide_String;
         Mode   : Window_Mode := Windowed;
         Rows   : Positive_Row_Count := 1;
         Cols   : Positive_Col_Count := 1;
      end record;

   function New_Dialog (
      Widget : in Widgets.Widget'Class;
      Title  : in Glyph_String       := "";
      Mode   : in Window_Mode        := Windowed;
      Rows   : in Positive_Row_Count := Default_Dialog_Height;
      Cols   : in Positive_Col_Count := Default_Dialog_Width)
      return Dialog is (
      Widget => Widgets.Holders.Create (Widget),
      Title  => To_Unbounded_Wide_Wide_String (Title),
      Mode   => Mode,
      Rows   => Rows,
      Cols   => Cols);

   function Get_Mode (
      Object : in Dialog)
      return Window_Mode is (
      Object.Mode);

end Malef.Dialogs;
