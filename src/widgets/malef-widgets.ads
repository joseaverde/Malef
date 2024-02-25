-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - W I D G E T S . A D S                     --
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

with Ada.Finalization;
with Malef.Surfaces;
with Malef.Styles.Classes;

package Malef.Widgets with Preelaborate is

   type Widget is abstract
      new Ada.Finalization.Controlled with
      private;

   type Draw_Area is
      record
         From : Cursor_Type;
         To   : Cursor_Type;
      end record;

   procedure On_Draw (
      Object  : in     Widget;
      Surface : in out Surfaces.Surface;
      Area    : in     Draw_Area) is
      abstract with
      Pre'Class => True;
   -- TODO: Check unchanged

   function Name (
      Object : in Widget)
      return Wide_Wide_String is
      abstract;

   function Id (
      Object : in Widget)
      return Wide_Wide_String is (
      "");

   function Classes (
      Object : in Widget)
      return Malef.Styles.Classes.Style_Class is (
      "");

   type Selectable is interface;

private

   type Widget is abstract
      new Ada.Finalization.Controlled with
      null record;

end Malef.Widgets;
