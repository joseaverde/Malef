-------------------------------------------------------------------------------
--                                                                           --
--              M A L E F - S T Y L E S - C L A S S E S . A D S              --
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

with Ada.Iterator_Interfaces;

private with Ada.Containers.Indefinite_Vectors;

package Malef.Styles.Classes with Preelaborate is

   -- TODO: #id .class widget

   type Style_Class is tagged private with
      Put_Image                 => Put_Image,
      String_Literal            => Value,
      Default_Iterator          => Iterate,
      Iterator_Element          => Wide_Wide_String,
      Constant_Indexing         => Constant_Reference,
      Default_Initial_Condition => Length (Style_Class) = 0;
   -- Each widget can be associated to one or many classes (in the later case,
   -- if two classes implement the same property then the latest will be
   -- chosen). Class names are case sensitive. You can define them using a
   -- String_Literal with the element separated by spaces:
   --
   --    My_Class : Style_Class := "main common";
   --
   -- You can iterate over the classes and check if an object belongs to
   -- certain class.
   --
   --    for Class of My_Class loop
   --       Put_Line (Class);
   --    end loop;
   --
   --    if My_Class ("main") then
   --       Put_Line ("My_Class contains main");
   --    end if;
   --
   -- You can join two classes together using the "+" operator:
   --
   --    Your_Class : Style_Class := "pretty main border";
   --    Our_Class  : Style_Class := My_Class + Your_Class;
   --    -- Our_Class := "main common pretty main border"
   --
   -- Keep in mind that having an elements that belongs to many classes kills
   -- performace, because classes are applyied one after the other.

   --<<--------->>--
   -->> Aspects <<--
   --<<--------->>--

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Style_Class);

   function Value (
      Item : in Wide_Wide_String)
      return Style_Class;

   type Cursor is private;

   function Has_Element (
      Item : in Cursor)
      return Boolean;

   package Style_Class_Iterator_Interfaces is
      new Ada.Iterator_Interfaces (
      Cursor      => Cursor,
      Has_Element => Has_Element);

   function Iterate (
      Item : aliased in Style_Class)
      return Style_Class_Iterator_Interfaces.Forward_Iterator'Class;

   function Constant_Reference (
      Object : in Style_Class;
      Name   : in Wide_Wide_String)
      return Boolean;

   function Constant_Reference (
      Object   : in Style_Class;
      Position : in Cursor)
      return Wide_Wide_String;

   --<<------------>>--
   -->> Operations <<--
   --<<------------>>--

   function Length (
      Class : in Style_Class)
      return Natural with
      Global => null;
   -- @param Class
   -- The Style_Class object that contains the different classes.
   --
   -- @return
   -- Returns the amount of classes that are contained withing the class object

   function Contains (
      Object : in Style_Class;
      Name   : in Wide_Wide_String)
      return Boolean;

   function "+" (Left, Right : in Style_Class)
      return Style_Class;

private

   package Class_Vectors is
      new Ada.Containers.Indefinite_Vectors (
      Index_Type   => Positive,
      Element_Type => Wide_Wide_String);

   type Style_Class is tagged
      record
         Classes : Class_Vectors.Vector;
      end record;

   type Cursor is new Class_Vectors.Cursor;

   overriding
   function Has_Element (
      Item : in Cursor)
      return Boolean is (
      Class_Vectors.Has_Element (Class_Vectors.Cursor (Item)));

   function Length (
      Class : in Style_Class)
      return Natural is (
      Natural (Class.Classes.Length));

   function Constant_Reference (
      Object : in Style_Class;
      Name   : in Wide_Wide_String)
      return Boolean is (
      Contains (Object, Name));

   function Constant_Reference (
      Object   : in Style_Class;
      Position : in Cursor)
      return Wide_Wide_String is (
      Object.Classes (Class_Vectors.Cursor (Position)));

   function Contains (
      Object : in Style_Class;
      Name   : in Wide_Wide_String)
      return Boolean is (
      (for some Item of Object.Classes => Item = Name));

   function "+" (Left, Right : in Style_Class)
      return Style_Class is (
      Classes => Class_Vectors."&" (Left.Classes, Right.Classes));

end Malef.Styles.Classes;
