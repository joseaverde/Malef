-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - W I N D O W S . A D S                     --
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

with Malef.Boxes;
pragma Elaborate (Malef.Boxes);

--
-- @summary
-- The Windows behave exactly like Boxes but have the advantage that they are
-- protected. The only drawback is that it's not possible to obtain any kind of
-- Window reference to put it into other Windows or inside a Box
--
-- @description
--
package Malef.Windows is

   pragma Elaborate_Body (Malef.Windows);

   -- TODO: Comment it
   protected type Window_Type is
      procedure Create (
         Height : Row_Type;
         Width  : Col_Type);

      function Contains (
         Layer : Malef.Boxes.Layer_Type)
         return Boolean;

      entry Draw;

      function Get_Position return Coord_Type;
      procedure Insert (
         Item  : Surface_Reference;
         Layer : Malef.Boxes.Layer_Type);

      procedure Move (
         Position : Coord_Type);

      procedure Remove (Layer : Malef.Boxes.Layer_Type);

      procedure Replace (
         Item  : Surface_Reference;
         Layer : Malef.Boxes.Layer_Type);

      procedure Resize (
         Height : Row_Type;
         Width  : Col_Type);

      entry Update;
   private
      procedure Check_Created with Inline;
      Box : Malef.Boxes.Box_Type;
      ID  : Integer := -1;

      Created  : Boolean := False;
      Updating : Boolean := False;
      Drawing  : Boolean := False;
      Resizing : Boolean := False;
   end Window_Type;

   -- type Window_Access is access Window_Type;
   Main_Window : Window_Type;

private

   Last_ID : Natural := 0;

end Malef.Windows;


---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
