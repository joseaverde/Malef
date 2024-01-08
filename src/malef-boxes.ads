-------------------------------------------------------------------------------
--                                                                           --
--                       M A L E F - B O X E S . A D S                       --
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

with Ada.Strings.Text_Buffers;
with Malef.Surfaces;

private with Ada.Containers.Indefinite_Holders;

package Malef.Boxes with Preelaborate is

   pragma Unevaluated_Use_Of_Old (Allow);

   type Layer_Mode is (
      None, Normal, Lighten, Screen, Dodge, Addition, Darken, Multiply, Burn,
      Overlay, Soft_Light, Hard_Light, Difference, Substract, Grain_Extract,
      Grain_Merge, Divide);
   -- The layer modes are based on the GIMP ones:
   --    <https://docs.gimp.org/en/gimp-concepts-layer-modes.html>

   type Layer_Opacity is new Float range 0.0 .. 1.0;

   type Layer_Count is new Natural;
   subtype Layer_Index is Layer_Count range 1 .. Layer_Count'Last;

   type Box (Capacity : Layer_Index) is
      tagged limited private with
      Aggregate                 => (Empty          => Empty,
                                    Append         => Append,
                                    New_Indexed    => New_Box,
                                    Assign_Indexed => Replace),
      Put_Image                 => Put_Image,
      Default_Initial_Condition =>
         Is_Empty (Box) and then
         (for all I in 1 .. Box.Capacity =>
            (not Contains (Box, I)));

   type Box_Access is access all Box'Class;
   type Surface_Access is access all Surfaces.Surface'Class;

   function Contains (
      Object : in Box;
      Layer  : in Layer_Index)
      return Boolean with
      Global => null;

   function Size (
      Object : in Box)
      return Natural with
      Post   => Size'Result <= Natural (Object.Capacity),
      Global => null;

   function Is_Empty (
      Object : in Box)
      return Boolean is (
      Object.Size = 0);

   --<<---------------->>--
   -->> Box Operations <<--
   --<<---------------->>--

   procedure Update (
      Object : in out Box) with
      Global => null;

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Box) with
      Global => null;

   --<<---------------------->>--
   -->> Container Operations <<--
   --<<---------------------->>--

   type Box_Item_Kind is (A_Surface, A_Box);

   type Box_Item (Kind : Box_Item_Kind) is
      record
         Position : Cursor_Type   := (1, 1);
         Mode     : Layer_Mode    := Normal;
         Opacity  : Layer_Opacity := 1.0;
         case Kind is
            when A_Surface => Surface  : not null Surface_Access;
            when A_Box     => Box      : not null Box_Access;
         end case;
      end record;

   function Item (
      Surface  : not null Surface_Access;
      Position : in Cursor_Type   := (1, 1);
      Mode     : in Layer_Mode    := Normal;
      Opacity  : in Layer_Opacity := 1.0)
      return Box_Item is (
      Kind     => A_Surface,
      Surface  => Surface,
      Position => Position,
      Mode     => Mode,
      Opacity  => Opacity);

   function Item (
      Box      : not null Box_Access;
      Position : in Cursor_Type   := (1, 1);
      Mode     : in Layer_Mode    := Normal;
      Opacity  : in Layer_Opacity := 1.0)
      return Box_Item is (
      Kind     => A_Box,
      Box      => Box,
      Position => Position,
      Mode     => Mode,
      Opacity  => Opacity);

   function Empty (Capacity : Layer_Index := 10)
      return Box with
      Post     => Empty'Result.Capacity = Capacity
         and then Is_Empty (Empty'Result)
         and then (for all I in 1 .. Empty'Result.Capacity =>
                     (not Contains (Empty'Result, I))),
      Global   => null;

   function Last_Index (
      Object : in Box)
      return Layer_Count is (
      [for I in 1 .. Object.Capacity =>
         (if Contains (Object, I) then I else 0)]'
         Reduce (Layer_Count'Max, 0));

   procedure Append (
      Object   : in out Box;
      New_Item : in     Box_Item;
      Index    : in     Layer_Index) with
      Pre      => Last_Index (Object) < Object.Capacity
         or else (raise Constraint_Error),
      Post     => Last_Index (Object) = Last_Index (Object)'Old + 1
         and then Contains (Object, Last_Index (Object))
         and then Size (Object) = Size (Object) + 1
         and then Get_Mode (Object, Last_Index (Object)) = New_Item.Mode
         and then Get_Opacity (Object, Last_Index (Object)) = New_Item.Opacity
         and then not Is_Hidden (Object, Last_Index (Object))
         and then Get_Position (Object, Last_Index (Object)) = New_Item.Position,
      Global   => null;

   function New_Box (First, Last : in Layer_Index)
      return Box is (
      Empty (Last - First + 1)) with
      Pre => First = Layer_Index'First;
   -- TODO, Set all elements to contain some dummy surface

   procedure Insert (
      Object   : in out Box;
      Index    : in     Layer_Index;
      Surface  : in     not null Surface_Access;
      Position : in     Cursor_Type   := (1, 1);
      Mode     : in     Layer_Mode    := Normal;
      Opacity  : in     Layer_Opacity := 1.0) with
      Pre   => not Contains (Object, Index) or else raise Constraint_Error,
      Post  => Contains (Object, Index)
               and then Size (Object) = Size (Object)'Old + 1
               and then Get_Mode (Object, Index) = Mode
               and then Get_Opacity (Object, Index) = Opacity
               and then not Is_Hidden (Object, Index)
               and then Get_Position (Object, Index) = Position,
      Global => null;

   procedure Insert (
      Object   : in out Box;
      Index    : in     Layer_Index;
      Other    : in     not null Box_Access;
      Position : in     Cursor_Type   := (1, 1);
      Mode     : in     Layer_Mode    := Normal;
      Opacity  : in     Layer_Opacity := 1.0) with
      Pre   => not Contains (Object, Index) or else raise Constraint_Error,
      Post  => Contains (Object, Index)
               and then Size (Object) = Size (Object)'Old + 1
               and then Get_Mode (Object, Index) = Mode
               and then Get_Opacity (Object, Index) = Opacity
               and then not Is_Hidden (Object, Index)
               and then Get_Position (Object, Index) = Position,
      Global => null;

   procedure Delete (
      Object : in out Box;
      Index  : in     Layer_Index) with
      Pre   => Contains (Object, Index) or else raise Constraint_Error,
      Post  => not Contains (Object, Index)
               and then Size (Object) = Size (Object)'Old + 1,
      Global => null;

   procedure Replace (
      Object   : in out Box;
      Index    : in     Layer_Index;
      New_Item : in     Box_Item);

   --<<------------------>>--
   -->> Layer Operations <<--
   --<<------------------>>--

   function Is_Hidden (
      Object : in Box;
      Index  : in Layer_Index)
      return Boolean with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Global => null;

   procedure Hide (
      Object : in out Box;
      Index  : in     Layer_Index) with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Post   => Is_Hidden (Object, Index),
      Global => null;

   procedure Show (
      Object : in out Box;
      Index  : in     Layer_Index) with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Post   => not Is_Hidden (Object, Index),
      Global => null;

   function Get_Mode (
      Object : in Box;
      Index  : in Layer_Index)
      return Layer_Mode with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Global => null;

   procedure Set_Mode (
      Object : in out Box;
      Index  : in     Layer_Index;
      Item   : in     Layer_Mode) with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Post   => Get_Mode (Object, Index) = Item,
      Global => null;

   function Get_Opacity (
      Object : in Box;
      Index  : in Layer_Index)
      return Layer_Opacity with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Global => null;

   procedure Set_Opacity (
      Object : in out Box;
      Index  : in     Layer_Index;
      Item   : in     Layer_Opacity) with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Post   => Get_Opacity (Object, Index) = Item,
      Global => null;

   function Get_Position (
      Object : in Box;
      Index  : in Layer_Index)
      return Cursor_Type with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Global => null;

   procedure Move_Layer (
      Object : in out Box;
      Index  : in Layer_Index;
      To     : in     Cursor_Type) with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Post   => Get_Position (Object, Index) = To,
      Global => null;

private

   type Layer_Type is
      record
         Surface  : Surface_Access := null;
         Box      : Box_Access     := null;
         Position : Cursor_Type    := (0, 0);
         Opacity  : Layer_Opacity  := 0.0;
         Mode     : Layer_Mode     := None;
         Hidden   : Boolean        := False;
         In_Use   : Boolean        := False;
      end record with
      Object_Size => 256;

   package Surface_Holders is
      new Ada.Containers.Indefinite_Holders (
      Element_Type => Surfaces.Surface,
      "="          => Surfaces."=");

   type Layer_Array is array (Layer_Index range <>) of Layer_Type;

   type Box (Capacity : Layer_Index) is
      tagged limited record
         Count   : Natural;
         Canvas  : Surface_Holders.Holder;
         Updated : Boolean := True;
         Layers  : Layer_Array (1 .. Capacity);
      end record;

   -->> Implementation <<--

   function Empty (Capacity : Layer_Index := 10)
      return Box is (
      Capacity => Capacity,
      Count    => 0,
      Canvas   => Surface_Holders.Empty_Holder,
      Updated  => True,
      Layers   => (others => <>));

   function Contains (
      Object : in Box;
      Layer  : in Layer_Index)
      return Boolean is (
      Object.Layers (Layer).In_Use);

   function Size (
      Object : in Box)
      return Natural is (
      Object.Count);

   function Is_Hidden (
      Object : in Box;
      Index  : in Layer_Index)
      return Boolean is (
      Object.Layers (Index).Hidden);

   function Get_Mode (
      Object : in Box;
      Index  : in Layer_Index)
      return Layer_Mode is (
      Object.Layers (Index).Mode);

   function Get_Opacity (
      Object : in Box;
      Index  : in Layer_Index)
      return Layer_Opacity is (
      Object.Layers (Index).Opacity);

   function Get_Position (
      Object : in Box;
      Index  : in Layer_Index)
      return Cursor_Type is (
      Object.Layers (Index).Position);

end Malef.Boxes;
