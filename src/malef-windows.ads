-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - W I N D O W S . A D S                     --
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

with Malef.Boxes;

package Malef.Windows with Preelaborate is

   type Box_Access is access all Boxes.Box;

   protected type Window (Capacity : Positive) is

      -- procedure Insert (
      --    Box      : in Box_Access;
      --    Index    : in Positive;
      --    Position : in Cursor_Type := (1, 1)) with
      --    Pre    => Index in 1 .. Capacity,
      --    Post   => Contains (Box, Index),
      --    Global => null;

      -- function Contains (
      --    Box   : in Box_Access;
      --    Index : in Positive)
      --    return Boolean with
      --    Pre    => Index in 1 .. Capacity,
      --    Global => null;

      -- procedure Draw with
      --    Global => null;

   --    function Get_Position return Coord_Type;
   --    procedure Insert (
   --       Item  : Surface_Reference;
   --       Layer : Malef.Boxes.Layer_Type);

   --    procedure Move (
   --       Position : Coord_Type);

   --    procedure Remove (Layer : Malef.Boxes.Layer_Type);

   --    procedure Replace (
   --       Item  : Surface_Reference;
   --       Layer : Malef.Boxes.Layer_Type);

   --    procedure Resize (
   --       Height : Row_Type;
   --       Width  : Col_Type);

   --    entry Update;
   -- private
   --    procedure Check_Created with Inline;
   --    Box : Malef.Boxes.Box_Type;
   --    ID  : Integer := -1;

   --    Created  : Boolean := False;
   --    Updating : Boolean := False;
   --    Drawing  : Boolean := False;
   --    Resizing : Boolean := False;
   end Window;

end Malef.Windows;
