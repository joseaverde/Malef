-------------------------------------------------------------------------------
--                                                                           --
--             M A L E F - W I D G E T S - H O L D E R S . A D S             --
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

private with Ada.Containers.Indefinite_Holders;
private with Ada.Finalization;

package Malef.Widgets.Holders with Preelaborate is

   type Holder is tagged private;

   type Widget_Reference_Type (
      Element : not null access Widget'Class) is
      limited null record with Implicit_Dereference => Element;

   type Widget_Constant_Reference_Type (
      Element : not null access constant Widget'Class) is
      limited null record with Implicit_Dereference => Element;

   function Create (
      Item : in Widget'Class)
      return Holder with
      Global => null;

   procedure Hold (
      Object : in out Holder;
      Item   : in     Widget'Class) with
      Global => null;

   procedure Release (
      Object : in out Holder) with
      Global => null;

   function Reference (
      Object : aliased in out Holder)
      return Widget_Reference_Type with
      Global => null;

   function Constant_Reference (
      Object : aliased in Holder)
      return Widget_Constant_Reference_Type with
      Global => null;

private

   package Widget_Holders is
      new Ada.Containers.Indefinite_Holders (
      Element_Type => Widget'Class);

   type Holder is
      new Ada.Finalization.Controlled with
      record
         Item : Widget_Holders.Holder;
      end record;

   overriding procedure Initialize (Object : in out Holder);

   function Create (
      Item : in Widget'Class)
      return Holder is (
      Ada.Finalization.Controlled with
      Item => Widget_Holders.To_Holder (Item));

   function Reference (
      Object : aliased in out Holder)
      return Widget_Reference_Type is (
      Element => Object.Item.Reference.Element);

   function Constant_Reference (
      Object : aliased in Holder)
      return Widget_Constant_Reference_Type is (
      Element => Object.Item.Constant_Reference.Element);

end Malef.Widgets.Holders;
