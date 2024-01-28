-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - G R O U P S . A D S                      --
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

private with Ada.Finalization;
private with System.Atomic_Operations.Test_and_Set;

package Malef.Groups is -- with Preelaborate is

   pragma Unevaluated_Use_Of_Old (Allow);

   type Layer_Mode is (None, Normal, Lighten, Screen, Dodge);

   type Layer_Opacity is new Float range 0.0 .. 1.0;

   type Layer_Count is new Natural;
   subtype Layer_Index is Layer_Count range 1 .. Layer_Count'Last;

   type Layer_Kind is (A_Surface, A_Group);

   type Group_Element (<>) is tagged private;

   function Contains (
      Object : in Group_Element)
      return Boolean with
      Global => null;

   function Kind (
      Object : in Group_Element)
      return Layer_Kind with
      Pre    => Contains (Object),
      Global => null;

   --<<-------->>--
   -->> Groups <<--
   --<<-------->>--

   type Group (
      Capacity : Layer_Index) is
      tagged private with
      Aggregate                 => (Empty          => Empty,
                                    Add_Unnamed    => Add_Unnamed,
                                    New_Indexed    => New_Group,
                                    Assign_Indexed => Assign_Indexed),
      Put_Image                 => Put_Image,
      Default_Initial_Condition => Default_Initial_Condition (Group);

   Default_Position  : constant Cursor_Type   := (1, 1);
   Opaque_Layer      : constant Layer_Opacity := Layer_Opacity'Last;
   Transparent_Layer : constant Layer_Opacity := Layer_Opacity'First;

   -->> Put Image <<--

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Group) with
      Global => null;
   -- This function implements the Put_Image aspects so you can use the
   -- 'Image, 'Wide_Image and 'Wide_Wide_Image attributes from the Group type.
   --
   -- The way the Group is represented may change in future versions. Therefore
   -- keep this routine for debugging only.
   --
   -- @param Buffer
   -- The Text_Buffer where to write the image representation of the Group.
   --
   -- @param Arg
   -- The 

   -->> As a Container <<--

   type Surface_Reference_Type (
      Element : not null access Surfaces.Surface) is
      limited null record with Implicit_Dereference => Element;

   type Surface_Constant_Reference_Type (
      Element : not null access constant Surfaces.Surface) is
      limited null record with Implicit_Dereference => Element;

   function Size (
      Object : in Group)
      return Layer_Count with
      Post   => Size'Result <= Object.Capacity,
      Global => null;

   function Is_Empty (
      Object : in Group)
      return Boolean is (
      Object.Size = 0);

   function Set_Surface (
      Object : aliased in out Group;
      Index  :         in     Layer_Index)
      return Surface_Reference_Type with
      Pre      => (Contains (Object, Index)
                   and then Kind (Object, Index) = A_Surface)
         or else  raise Constraint_Error,
      Global   => null;

   function Get_Surface (
      Object : aliased in Group;
      Index  :         in Layer_Index)
      return Surface_Constant_Reference_Type with
      Pre      => (Contains (Object, Index)
                   and then Kind (Object, Index) = A_Surface)
         or else  raise Constraint_Error,
      Global => null;

   -- TODO: Get_Group
   -- TODO: Set_Group
   -- TODO: Insert

   procedure Clear (
      Object : in out Group) with
      Post   => Default_Initial_Condition (Object),
      Global => null;

   procedure Delete (
      Object : in out Group;
      Index  : in     Layer_Index);
   -- TODO: Contracts

   -->> As a Composer <<--

   procedure Update (
      Object : in out Group) with
      Global => null;

   function See_Surface (
      Object : aliased in Group)
      return Surface_Constant_Reference_Type with
      Global => null;

   function Rows (
      Object : in Group)
      return Row_Count with
      Global => null;

   function Cols (
      Object : in Group)
      return Col_Count with
      Global => null;

   -->> As an Aggregate <<--

   function Element (
      Surface  : in Surfaces.Surface;
      Position : in Cursor_Type   := Default_Position;
      Mode     : in Layer_Mode    := Normal;
      Hidden   : in Boolean       := False;
      Opacity  : in Layer_Opacity := Opaque_Layer)
      return Group_Element'Class with
      Post     => Contains (Element'Result)
         and then Kind (Element'Result) = A_Surface,
      Global => null;

   function Element (
      Rows     : in Positive_Row_Count;
      Cols     : in Positive_Col_Count;
      Position : in Cursor_Type   := Default_Position;
      Mode     : in Layer_Mode    := Normal;
      Hidden   : in Boolean       := False;
      Opacity  : in Layer_Opacity := Opaque_Layer)
      return Group_Element'Class with
      Post     => Contains (Element'Result)
         and then Kind (Element'Result) = A_Surface,
      Global => null;

   function Element (
      Group    : in Groups.Group;
      Position : in Cursor_Type   := Default_Position;
      Mode     : in Layer_Mode    := Normal;
      Hidden   : in Boolean       := False;
      Opacity  : in Layer_Opacity := Opaque_Layer)
      return Group_Element'Class with
      Post     => Contains (Element'Result)
         and then Kind (Element'Result) = A_Surface,
      Global   => null;

   -- TODO: Move constructor for Groups

   function No_Layer
      return Group_Element'Class with
      Post   => not Contains (No_Layer'Result),
      Global => null;

   function Empty (
      Capacity : in Layer_Index := 10)
      return Group with
      Post     => Empty'Result.Capacity = Capacity
         and then Default_Initial_Condition (Empty'Result),
      Global   => null;

   procedure Add_Unnamed (
      Object   : in out Group;
      New_Item : in     Group_Element'Class) with
      Pre      => Last_Index (Object) < Object.Capacity
         or else (raise Constraint_Error),
      Post     => Last_Index (Object) = Last_Index (Object)'Old + 1
         and then Contains (Object, Last_Index (Object)) = Contains (New_Item)
         and then (if Contains (New_Item)
                     then        Size (Object) = Size (Object)'Old + 1
                        and then Kind (Object, Last_Index (Object))
                                 = Kind (New_Item)
                     else Size (Object) = Size (Object)'Old),
      Global   => null;

   function New_Group (
      First : in Layer_Index;
      Last  : in Layer_Index)
      return Group is (
      Empty (Last - First + 1)) with
      Pre => First = Layer_Index'First and then Last - First + 1 > 0;

   procedure Assign_Indexed (
      Object   : in out Group;
      Index    : in     Layer_Index;
      New_Item : in     Group_Element'Class) with
      Pre      => Index in 1 .. Object.Capacity or else raise Constraint_Error,
      Global   => null;
   -- TODO: Add same-as postcondition

   -->> Layer Operations <<--

   function Contains (
      Object : in Group;
      Index  : in Layer_Index)
      return Boolean with
      Global => null;

   function Kind (
      Object : in Group;
      Index  : in Layer_Index)
      return Layer_Kind with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Global => null;

   function Is_Hidden (
      Object : in Group;
      Index  : in Layer_Index)
      return Boolean with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Global => null;

   procedure Hide (
      Object : in out Group;
      Index  : in     Layer_Index) with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Post   => Is_Hidden (Object, Index),
      Global => null;

   procedure Show (
      Object : in out Group;
      Index  : in     Layer_Index) with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Post   => not Is_Hidden (Object, Index),
      Global => null;

   function Get_Mode (
      Object : in Group;
      Index  : in Layer_Index)
      return Layer_Mode with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Global => null;

   procedure Set_Mode (
      Object : in out Group;
      Index  : in     Layer_Index;
      Item   : in     Layer_Mode) with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Post   => Get_Mode (Object, Index) = Item,
      Global => null;

   function Get_Opacity (
      Object : in Group;
      Index  : in Layer_Index)
      return Layer_Opacity with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Global => null;

   procedure Set_Opacity (
      Object : in out Group;
      Index  : in     Layer_Index;
      Item   : in     Layer_Opacity) with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Post   => Get_Opacity (Object, Index) = Item,
      Global => null;

   function Get_Position (
      Object : in Group;
      Index  : in Layer_Index)
      return Cursor_Type with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Global => null;

   procedure Move_Layer (
      Object : in out Group;
      Index  : in     Layer_Index;
      To     : in     Cursor_Type) with
      Pre    => Contains (Object, Index) or else raise Constraint_Error,
      Post   => Get_Position (Object, Index) = To,
      Global => null;

   -->> Helper Functions <<--

   function Same_As (
      Object   : in Group;
      Index    : in Layer_Index;
      Position : in Cursor_Type;
      Mode     : in Layer_Mode;
      Hidden   : in Boolean;
      Opacity  : in Layer_Opacity)
      return Boolean is (
               Get_Position (Object, Index) = Position
      and then Is_Hidden (Object, Index)    = Hidden
      and then Get_Mode (Object, Index)     = Mode
      and then Get_Opacity (Object, Index)  = Opacity) with
      Pre => Contains (Object, Index) or else raise Constraint_Error;
   -- This helper function returns True if the element has the given
   -- attributes. It is used in postconditions to tell the element has been
   -- inserted well.

   function Last_Index (
      Object : in Group)
      return Layer_Count is (
      [for I in 1 .. Object.Capacity =>
         (if Contains (Object, I) then I else 0)]'
         Reduce (Layer_Count'Max, 0));

   function Add_Unnamed_Index (
      Object : in Group)
      return Layer_Count with
      Global => null;

   function Default_Initial_Condition (
      Group : in Groups.Group)
      return Boolean is (
      Rows (Group) = 0 and then
      Cols (Group) = 0 and then
      Is_Empty (Group) and then
      (for all I in 1 .. Group.Capacity =>
         (not Contains (Group, I))));

private

   --<<-------->>--
   -->> Layers <<--
   --<<-------->>--

   Default_Rows : constant := 24;
   Default_Cols : constant := 80;

   type Surface_Access is access Surfaces.Surface;
   type Group_Access is access Group;

   type Layer_Type (
      Kind : Layer_Kind) is
      record
         Position : Cursor_Type   := (0, 0);
         Opacity  : Layer_Opacity := 0.0;
         Mode     : Layer_Mode    := None;
         Hidden   : Boolean       := True;
         case Kind is
            when A_Surface => Surface : not null Surface_Access;
            when A_Group   => Group   : not null Group_Access;
         end case;
      end record;

   type Layer_Access is access Layer_Type;

   type Layer_Array is array (Layer_Index range <>) of Layer_Access;

   --<<---------------->>--
   -->> Group Elements <<--
   --<<---------------->>--

   package Test_And_Set renames System.Atomic_Operations.Test_And_Set;

   subtype Atomic_Boolean is Test_And_Set.Test_And_Set_Flag;
   type Atomic_Counter is new Integer with Atomic;

   type Control_Type is
      record
         Counter : aliased Atomic_Counter;
         Owned   : aliased Atomic_Boolean;
      end record;

   type Control_Access is access Control_Type;

   function New_Control
      return Control_Access is (
      new Control_Type'(Counter => 1, Owned => 0));

   type Group_Element is
      new Ada.Finalization.Controlled with
      record
         Layer   : Layer_Access := null;
         Control : Control_Access := null;
      end record;

   overriding
   procedure Adjust (
      Object : in out Group_Element);

   overriding
   procedure Finalize (
      Object : in out Group_Element);

   function Contains (
      Object : in Group_Element)
      return Boolean is (
      Object.Layer /= null);

   function Kind (
      Object : in Group_Element)
      return Layer_Kind is (
      Object.Layer.Kind);

   --<<-------->>--
   -->> Groups <<--
   --<<-------->>--

   type Group (
      Capacity : Layer_Index) is
      new Ada.Finalization.Controlled with
      record
         Count   : Layer_Count := 0;
         Index   : Layer_Count := 0;
         Updated : Boolean := False;
         Surface : Surface_Access
                 := new Surfaces.Surface (Default_Rows, Default_Cols);
         Layers  : Layer_Array (1 .. Capacity) := (others => null);
         Region  : Cursor_Type := (0, 0);
      end record;

   function Deep_Copy (
      Object : in Group)
      return Group_Access;

   function Size (
      Object : in Group)
      return Layer_Count is (
      Object.Count);

   -->> As a Container <<--

   function Get_Surface (
      Object : aliased in Group;
      Index  :         in Layer_Index)
      return Surface_Constant_Reference_Type is (
      Element => Object.Layers (Index).Surface);

   -->> As a Composer <<--

   function See_Surface (
      Object : aliased in Group)
      return Surface_Constant_Reference_Type is (
      Element => Object.Surface);

   function Rows (
      Object : in Group)
      return Row_Count is (
      Object.Region.Row);

   function Cols (
      Object : in Group)
      return Col_Count is (
      Object.Region.Col);

   -->> As an Aggregate <<--

   function Deep_Copy (
      Object : in Group)
      return Group_Access is (
      new Group'(Object));

   function Element (
      Surface  : in Surfaces.Surface;
      Position : in Cursor_Type   := Default_Position;
      Mode     : in Layer_Mode    := Normal;
      Hidden   : in Boolean       := False;
      Opacity  : in Layer_Opacity := Opaque_Layer)
      return Group_Element'Class is (
      Group_Element'(Ada.Finalization.Controlled with
      Layer  => new Layer_Type'(
         Kind     => A_Surface,
         Position => Position,
         Opacity  => Opacity,
         Mode     => Mode,
         Hidden   => Hidden,
         Surface  => new Surfaces.Surface'(Surface),
         others   => <>),
      Control => New_Control));

   function Element (
      Rows     : in Positive_Row_Count;
      Cols     : in Positive_Col_Count;
      Position : in Cursor_Type   := Default_Position;
      Mode     : in Layer_Mode    := Normal;
      Hidden   : in Boolean       := False;
      Opacity  : in Layer_Opacity := Opaque_Layer)
      return Group_Element'Class is (
      Group_Element'(Ada.Finalization.Controlled with
      Layer  => new Layer_Type'(
         Kind     => A_Surface,
         Position => Position,
         Opacity  => Opacity,
         Mode     => Mode,
         Hidden   => Hidden,
         Surface  => new Surfaces.Surface (Rows, Cols),
         others   => <>),
      Control => New_Control));

   function Element (
      Group    : in Groups.Group;
      Position : in Cursor_Type   := Default_Position;
      Mode     : in Layer_Mode    := Normal;
      Hidden   : in Boolean       := False;
      Opacity  : in Layer_Opacity := Opaque_Layer)
      return Group_Element'Class is (
      Group_Element'(Ada.Finalization.Controlled with
      Layer  => new Layer_Type'(
         Kind     => A_Group,
         Position => Position,
         Opacity  => Opacity,
         Mode     => Mode,
         Hidden   => Hidden,
         Group    => Deep_Copy (Group),
         others   => <>),
      Control => New_Control));

   function No_Layer
      return Group_Element'Class is (
      Group_Element'(Ada.Finalization.Controlled with
      Layer   => null,
      Control => null));

   function Empty (
      Capacity : in Layer_Index := 10)
      return Group is (
      Ada.Finalization.Controlled with
      Capacity => Capacity,
      others   => <>);

   -->> Layer Operations <<--

   function Contains (
      Object : in Group;
      Index  : in Layer_Index)
      return Boolean is (
      Index in 1 .. Object.Capacity and Object.Layers (Index) /= null);

   function Kind (
      Object : in Group;
      Index  : in Layer_Index)
      return Layer_Kind is (
      Object.Layers (Index).Kind);

   function Is_Hidden (
      Object : in Group;
      Index  : in Layer_Index)
      return Boolean is (
      Object.Layers (Index).Hidden);

   function Get_Mode (
      Object : in Group;
      Index  : in Layer_Index)
      return Layer_Mode is (
      Object.Layers (Index).Mode);

   function Get_Opacity (
      Object : in Group;
      Index  : in Layer_Index)
      return Layer_Opacity is (
      Object.Layers (Index).Opacity);

   function Get_Position (
      Object : in Group;
      Index  : in Layer_Index)
      return Cursor_Type is (
      Object.Layers (Index).Position);

   function Add_Unnamed_Index (
      Object : in Group)
      return Layer_Count is (
      Object.Index);

end Malef.Groups;
