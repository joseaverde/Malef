---------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - S T Y L E S . A D S                      --
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

private with Ada.Containers.Indefinite_Vectors;

package Malef.Styles with Preelaborate is

   -- This type provides a simple way of specifying the style for your
   -- application using Ada or a CSS-like language.

   --<<--------------->>--
   -->> Style Classes <<--
   --<<--------------->>--

   type Class_Type is tagged private with
      Put_Image                 => Put_Image,
      String_Literal            => Value,
      Constant_Indexing         => Constant_Reference,
      Default_Initial_Condition => Length (Class_Type) = 1;
   -- Each widget can be associated to one or many classes (in the later case,
   -- if two classes implement the same property then the latest will be
   -- chosen). Class names are case sensitive. You can define them using a
   -- String_Literal with the element separated by spaces:
   --
   --    My_Class : Class_Type := "main common";
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
   --    Your_Class : Class_Type := "pretty main border";
   --    Our_Class  : Class_Type := My_Class + Your_Class;
   --    -- Our_Class := "main common pretty main border"
   --
   -- Keep in mind that having an elements that belongs to many classes kills
   -- performace, because classes are applyied one after the other.

   function Length (
      Class : in Class_Type)
      return Natural with
      Global => null;
   -- @param Class
   -- The Class_Type object that contains the different classes.
   --
   -- @return
   -- Returns the amount of classes that are contained withing the class object

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Class_Type);

   function Value (
      Item : in Wide_Wide_String)
      return Class_Type;

   type Boolean_Constant_Reference_Type (
      Element : not null access constant Boolean) is
      limited null record with Implicit_Dereference => Element;

   function Constant_Reference (
      Object : in Class_Type;
      Name   : in Wide_Wide_String)
      return Boolean_Constant_Reference_Type;

   function Contains (
      Object : in Class_Type;
      Name   : in Wide_Wide_String)
      return Boolean;

   function "+" (Left, Right : in Class_Type)
      return Class_Type;

   --<<------------>>--
   -->> Style Type <<--
   --<<------------>>--

   type Style_Field is (

      -->> General <<--

      Background,

      -->> Borders <<--

      Border_Foreground, Border_Background, Border_Style, Border_Thickness,

      -->> Text <<--

      Text_Background, Text_Foreground);

   type Style_Type is tagged private;

   -- procedure Load (Stream : not null access Root_Stream_Type'Class);
   -- procedure Load (File : in String);
   -- procedure Dump (Stream : not null access Root_Stream_Type'Class);
   -- procedure Dump (File : in String);
   -- function "+" (Left, Right : in Style_Type);
   -- Subpackage Malef.Styles.Watcher that updates a style if the file has
   -- changed.
   --
   -- Example usage:
   --
   --    [Text_Color     => "#F02312",
   --     Text_Alignment => Align_Left];
   --
   -- .css
   --
   --    Button {
   --       text-color     : #F02312;
   --       text-alignment : left-align;
   --    }

private

   --<<--------------->>--
   -->> Style Classes <<--
   --<<--------------->>--

   package Class_Vectors is
      new Ada.Containers.Indefinite_Vectors (
      Index_Type   => Positive,
      Element_Type => Wide_Wide_String);

   Aliased_True  : aliased constant Boolean := True;
   Aliased_False : aliased constant Boolean := False;

   type Class_Type is tagged
      record
         Classes : Class_Vectors.Vector;
      end record;

   function Length (
      Class : in Class_Type)
      return Natural is (
      Natural (Class.Classes.Length));

   function Constant_Reference (
      Object : in Class_Type;
      Name   : in Wide_Wide_String)
      return Boolean_Constant_Reference_Type is (
      (if Contains (Object, Name)
         then (Element => Aliased_True'Access)
         else (Element => Aliased_False'Access)));

   function Contains (
      Object : in Class_Type;
      Name   : in Wide_Wide_String)
      return Boolean is (
      (for some Item of Object.Classes => Item = Name));

   function "+" (Left, Right : in Class_Type)
      return Class_Type is (
      Classes => Class_Vectors."&" (Left.Classes, Right.Classes));

   --<<------------>>--
   -->> Style Type <<--
   --<<------------>>--

   type Style_Type is tagged null record;

end Malef.Styles;
