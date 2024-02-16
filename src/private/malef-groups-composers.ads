-------------------------------------------------------------------------------
--                                                                           --
--            M A L E F - G R O U P S - C O M P O S E R S . A D S            --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   S P E C                              --
--                                                                           --
-------------------------------------------------------------------------------
-- Copyright (c) 2021 - 2024  José Antonio Verde Jiménez All Rights Reserved --
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

private package Malef.Groups.Composers with Preelaborate is

   type Composer_Type is
      not null access
      procedure (
         Object : in out Group;
         Index  : in     Layer_Index;
         Offset : in     Cursor_Type);

   function Get_Composer (
      Mode : in Layer_Mode)
      return Composer_Type;

private

   generic
      with function Add (Top, Bottom : in Float) return Float;
   function Simple_Addition (Top, Bottom : in RGBA_Type)
      return RGBA_Type;

   type Weights is
      record
         Alpha  : Component_Type;
         Top    : Float;
         Bottom : Float;
      end record;

   generic
      with function Adder (Top, Bottom : in RGBA_Type) return RGBA_Type;
   procedure Compose (
      Object : in out Group;
      Index  : in     Layer_Index;
      Offset : in     Cursor_Type);

end Malef.Groups.Composers;
