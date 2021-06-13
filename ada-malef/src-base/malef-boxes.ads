-------------------------------------------------------------------------------
--                                                                           --
--                       M A L E F - B O X E S . A D S                       --
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

private with Ada.Containers.Ordered_Maps;

--TODO: Document
-- @summary
--
--
-- @description
--
package Malef.Boxes is

   type Layer_Type is new Positive;

   type Box_Type is new Base_Type with private;


   function Contains (Box   : in Box_Type;
                      Layer : Layer_Type)
                      return Boolean;

   procedure Insert (Box   : in out Box_Type;
                     Item  : Shared_Surface_Access;
                     Layer : Layer_Type);

   procedure Remove (Box   : in out Box_Type;
                     Layer : Layer_Type);

   procedure Replace (Box   : in out Box_Type;
                      Item  : Shared_Surface_Access;
                      Layer : Layer_Type);

   overriding
   procedure Resize (Box    : in out Box_Type;
                     Height : Row_Type;
                     Width  : Col_Type);

   procedure Set_Resizable (Box  : in out Box_Type;
                            Mode : Boolean := True);

   overriding
   procedure Update (Box : in out Box_Type);

private

   type Layer_Element is
      record
         Object : Shared_Surface_Access := Shared_Null_Surface'Access;
         Is_New : Boolean  := True;
      end record;

   package Object_Maps is new Ada.Containers.Ordered_Maps (
      Key_Type     => Layer_Type,
      Element_Type => Layer_Element
   );
   type Box_Type is new Base_Type with
      record
         Layers    : Object_Maps.Map;
         Resizable : Boolean := True;
      end record;


end Malef.Boxes;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
