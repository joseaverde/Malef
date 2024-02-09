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

with Malef.Surfaces;

private with Ada.Finalization;
private with Malef.Counters;

package Malef.Groups with Preelaborate is

   -- A Group is a container that can hold Surfaces or other Groups. It is
   -- similar to groups you can create on GIMP to group different layers
   -- together. Groups are constrained to a given capacity, but unlike a vector
   -- you can insert a Layer in position 3 without being another surface in
   -- positions 1 or 2.
   --
   -- You can use new Aggregate syntax to create groups:
   --
   --    declare
   --       Red       : Surface (3, 4);
   --       Green     : Surface (4, 3);
   --       Blue      : Surface (3, 3);
   --       RGB_Group : Group (3) :=
   --          [Layer (Red), Layer (Green), Layer (Blue)];
   --    begin
   --       -- Code
   --    end;
   --
   -- Also you can use numbers to indicate the position:
   --
   --    declare
   --       Red       : Surface (3, 4);
   --       Green     : Surface (4, 3);
   --       Blue      : Surface (3, 3);
   --       My_Group  : Group (6) := [4 => Layer (Red),
   --                                 1 => Layer (Green),
   --                                 6 => Layer (Blue),
   --                                 2 .. 3 | 5 => No_Layer];
   --    begin
   --       -- Code
   --    end;
   --
   -- However, keep in mind that doing it that way makes a copy of the Surface.
   -- Modifying `Red`, `Green` or `Blue`, won't change the group. If you want
   -- to modify it you can use Ada `renames` clause. You can also create the
   -- surfaces in place without declaring them before hand:
   --
   --    declare
   --       Red      : Surface (3, 4);
   --       My_Group : Group (3) := [Layer (Red), Layer (4, 3), Layer (3, 3)];
   --       Green renames My_Group.Set_Surface (2).Element;
   --       Blue renames My_Group.Set_Surface (3).Element;
   --    begin
   --       -- Modifying Red won't change the Group, but Green or Blue will.
   --    end;
   --
   -- When adding a layer you can specify, the position, the layer mode (which
   -- function should be used to merge it with the one below), and if whether
   -- it is hidden. By default the position is (1, 1), the layer mode is
   -- Normal, Opacity is 1.0 (completely opaque) and is shown (not hidden).
   -- For instance, you can do:
   --
   --    declare
   --       Red      : Surface (3, 4);
   --       My_Group : Group (3) := [Layer (Red, (1, 2)),   -- Position (1, 2)
   --                                Layer (Red, Opacity => 0.3),
   --                                Layer (Red, (1, 1), Opacity => 0.7),
   --                                Layer (Red, Hidden => True)];
   --    begin
   --       null;
   --    end;
   --
   -- Obviously you can add groups inside other groups:
   --
   --    declare
   --       Group : Group (3) := [Layer (1, 1),
   --                             Layer ([Layer (2, 2),
   --                                     Layer (3, 3)]),
   --                             Layer ([Layer (4, 4)])];
   --    begin
   --       null;
   --    end;
   --
   -- However, when you add group, its contents are copied which may be slow
   -- if done too much. Therefore we introduce the Move operation. It is
   -- similar to C++ `std::move` function in which it doesn't copy the original
   -- object, moves its contents to the other one and leaves the original
   -- object in an unusable but destroyable state. In other words, destroys the
   -- original object and moves its contents.
   --
   --    declare
   --       Group_A : Group (3) := [Layer (1, 2), No_Layer, Layer (3, 3)];
   --       Group_B : Group (2) := [Move (Group_A),
   --                               Move (Layer ([Layer (1, 2),
   --                                             Layer (2, 3),
   --                                             Layer (3, 4)]))];
   --    begin
   --       null;
   --    end;
   --
   -- You also have the `Insert`, `Delete`, `Clear` and `Move_Out_Group`
   -- functions to modify the contents of the container once it's created.
   --
   --  * `Insert` adds a layer to a given position, if something was there, it
   --     is destroyed. You can check if it exists with the `Contains` function
   --  * `Delete` removes a layer from a given position.
   --  * `Clear` removes all layers.
   --  * `Move_Out_Group` is similar to the `Move` function but it takes a
   --    group from another group and removes it from it.
   --
   -- You also have the `Get_Surface`, `Set_Surface`, `Get_Group` and
   -- `Set_Group` functions. All of them return either Constant References or
   -- Variable References to Surfaces or Groups. Tampering rules apply, you
   -- can't modify a group while you have a reference to an element. The only
   -- difference is that `Set_` return variable references and `Get_` return
   -- constant references.

   -- TODO: Forbid tampering with groups

   -- TODO: Allow changing the Group Palette

   pragma Unevaluated_Use_Of_Old (Allow);

   type Layer_Mode is (None, Normal, Lighten, Screen, Dodge);
   -- The Layer mode identifyies a function used to combine different layers
   -- together. Some functions are more expensive than others.
   --
   -- @enum None
   -- The simplest and fastest function, if the layer above is not fully
   -- transparent it takes it. Otherwise it takes the layer below.
   --
   -- @enum Normal
   -- With this function, it the top colour is not fully opaque it adds it
   -- proportionally to the colour in the layer below.

   type Layer_Opacity is new Float range 0.0 .. 1.0;

   type Layer_Count is new Natural;
   subtype Layer_Index is Layer_Count range 1 .. Layer_Count'Last;

   type Layer_Kind is (A_Surface, A_Group);

   type Group_Layer (<>) is tagged private;

   function Contains (
      Object : in Group_Layer)
      return Boolean with
      Global => null;

   function Kind (
      Object : in Group_Layer)
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
   -- The Group

   -->> As a Container <<--

   type Surface_Reference_Type (
      Element : not null access Surfaces.Surface) is
      limited null record with Implicit_Dereference => Element;

   type Surface_Constant_Reference_Type (
      Element : not null access constant Surfaces.Surface) is
      limited null record with Implicit_Dereference => Element;

   type Group_Reference_Type (
      Element : not null access Group) is
      limited null record with Implicit_Dereference => Element;

   type Group_Constant_Reference_Type (
      Element : not null access constant Group) is
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

   function Get_Group (
      Object : aliased in out Group;
      Index  :         in     Layer_Index)
      return Group_Reference_Type with
      Pre      => (Contains (Object, Index)
                   and then Kind (Object, Index) = A_Group)
         or else  raise Constraint_Error,
      Global => null;

   function Set_Group (
      Object : aliased in out Group;
      Index  :         in     Layer_Index)
      return Group_Reference_Type with
      Pre      => (Contains (Object, Index)
                   and then Kind (Object, Index) = A_Group)
         or else  raise Constraint_Error,
      Global => null;

   procedure Insert (
      Object   : in out Group;
      Index    : in     Layer_Index;
      Surface  : in     Surfaces.Surface;
      Position : in     Cursor_Type   := Default_Position;
      Mode     : in     Layer_Mode    := Normal;
      Hidden   : in     Boolean       := False;
      Opacity  : in     Layer_Opacity := Opaque_Layer);
   -- TODO: Contracts

   procedure Insert (
      Object   : in out Group;
      Index    : in     Layer_Index;
      Group    : in     Groups.Group;
      Position : in     Cursor_Type   := Default_Position;
      Mode     : in     Layer_Mode    := Normal;
      Hidden   : in     Boolean       := False;
      Opacity  : in     Layer_Opacity := Opaque_Layer);
   -- TODO: Contracts

   procedure Clear (
      Object : in out Group) with
      Post   => Default_Initial_Condition (Object),
      Global => null;

   procedure Delete (
      Object : in out Group;
      Index  : in     Layer_Index);
   -- TODO: Contracts

   -- function Move_Out_Group (
   --    Object : in out Group;
   --    Index  : in     Layer_Index)
   --    return Group_Layer'Class;
   -- TODO: Contracts

   -->> As a Composer <<--

   procedure Update (
      Object : in out Group) with
      Global => null;

   function See_Surface (
      Object : aliased in Group)
      return Surface_Constant_Reference_Type with
      Global => null;

   function Row (
      Object : in Group)
      return Row_Count with
      Global => null;

   function Col (
      Object : in Group)
      return Col_Count with
      Global => null;

   -- procedure Resize_Layer (
   --    Object   : in out Group;
   --    Index    : in     Layer_Index;
   --    New_Rows : in     Positive_Row_Count;
   --    New_Cols : in     Positive_Col_Count) with
   --    Pre      =>         (Valid (Group)
   --                and then Contains (Group, Index)
   --                and then Kind (Group, Index) = A_Surface)
   --       or else  raise Constraint_Error;
   -- TODO: Finish Contracts

   -->> As an Aggregate <<--

   function Layer (
      Surface  : in Surfaces.Surface;
      Position : in Cursor_Type   := Default_Position;
      Mode     : in Layer_Mode    := Normal;
      Hidden   : in Boolean       := False;
      Opacity  : in Layer_Opacity := Opaque_Layer)
      return Group_Layer'Class with
      Post     => Contains (Layer'Result)
         and then Kind (Layer'Result) = A_Surface,
      Global => null;

   function Layer (
      Rows     : in Positive_Row_Count;
      Cols     : in Positive_Col_Count;
      Position : in Cursor_Type   := Default_Position;
      Mode     : in Layer_Mode    := Normal;
      Hidden   : in Boolean       := False;
      Opacity  : in Layer_Opacity := Opaque_Layer)
      return Group_Layer'Class with
      Post     => Contains (Layer'Result)
         and then Kind (Layer'Result) = A_Surface,
      Global => null;

   function Layer (
      Group    : in Groups.Group;
      Position : in Cursor_Type   := Default_Position;
      Mode     : in Layer_Mode    := Normal;
      Hidden   : in Boolean       := False;
      Opacity  : in Layer_Opacity := Opaque_Layer)
      return Group_Layer'Class with
      Post     => Contains (Layer'Result)
         and then Kind (Layer'Result) = A_Group,
      Global   => null;

   function Move (
      Group    : in out Groups.Group;
      Position : in     Cursor_Type   := Default_Position;
      Mode     : in     Layer_Mode    := Normal;
      Hidden   : in     Boolean       := False;
      Opacity  : in     Layer_Opacity := Opaque_Layer)
      return Group_Layer'Class with
      Pre      => Valid (Group) or else raise Constraint_Error,
      Post     => Contains (Move'Result) and then Kind (Move'Result) = A_Group,
      Global   => null;
   -- About Move functions see the comment on the top of this package spec.

   function No_Layer
      return Group_Layer'Class with
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
      New_Item : in     Group_Layer'Class) with
      Pre      => Add_Unnamed_Index (Object) < Object.Capacity
         or else (raise Constraint_Error),
      Post     => Add_Unnamed_Index (Object)
                  = Add_Unnamed_Index (Object)'Old + 1
         and then Contains (Object, Add_Unnamed_Index (Object))
                  = Contains (New_Item)
         and then (if Contains (New_Item)
                     then        Size (Object) = Size (Object)'Old + 1
                        and then Kind (Object, Add_Unnamed_Index (Object))
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
      New_Item : in     Group_Layer'Class) with
      Pre      => Index in 1 .. Object.Capacity or else raise Constraint_Error,
      Global   => null;
   -- TODO: Add same-as postcondition

   procedure Insert (
      Object : in out Group;
      Index  : in     Layer_Index;
      Item   : in     Group_Layer'Class)
      renames Assign_Indexed;

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

   function Valid (
      Object : in Group)
      return Boolean with
      Global => null;

   function Default_Initial_Condition (
      Group : in Groups.Group)
      return Boolean is (
      Valid (Group)    and then
      Row (Group) = 0  and then
      Col (Group) = 0  and then
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

   function New_Counter
      return Counters.Counter_Access is (
      new Counters.Counter_Type'(Counter => 1, Locked => 0));

   type Group_Layer is
      new Ada.Finalization.Controlled with
      record
         Layer   : Layer_Access            := null;
         Counter : Counters.Counter_Access := null;
      end record;

   overriding
   procedure Adjust (
      Object : in out Group_Layer);

   overriding
   procedure Finalize (
      Object : in out Group_Layer);

   function Contains (
      Object : in Group_Layer)
      return Boolean is (
      Object.Layer /= null);

   function Kind (
      Object : in Group_Layer)
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
         Counter : Counters.Counter_Access := New_Counter;
      end record;

   overriding
   procedure Adjust (
      Object : in out Group);

   overriding
   procedure Finalize (
      Object : in out Group);

   function Internal_Surface (
      Object : in Group;
      Index  : in Layer_Index)
      return Surface_Access;

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

   function Get_Group (
      Object : aliased in out Group;
      Index  :         in     Layer_Index)
      return Group_Reference_Type is (
      Element => Object.Layers (Index).Group);

   -->> As a Composer <<--

   function See_Surface (
      Object : aliased in Group)
      return Surface_Constant_Reference_Type is (
      Element => Object.Surface);

   function Row (
      Object : in Group)
      return Row_Count is (
      Object.Region.Row);

   function Col (
      Object : in Group)
      return Col_Count is (
      Object.Region.Col);

   -->> As an Aggregate <<--

   function Deep_Copy (
      Object : in Group)
      return Group_Access is (new Group'(
      Object with delta
      Updated => False,
      Surface => new Surfaces.Surface'(Object.Surface.all),
      Layers  => [for I in Object.Layers'Range =>
                    (if Object.Layers (I) = null
                       then null
                     elsif Object.Layers (I).Kind = A_Surface
                       then new Layer_Type'(Object.Layers (I).all with delta
                                            Surface => new Surfaces.Surface'(
                                             Object.Layers (I).Surface.all))
                     else new Layer_Type'(Object.Layers (I).all with delta
                                          Group => Deep_Copy (
                                             Object.Layers (I).Group.all)))]));

   function Layer (
      Surface  : in Surfaces.Surface;
      Position : in Cursor_Type   := Default_Position;
      Mode     : in Layer_Mode    := Normal;
      Hidden   : in Boolean       := False;
      Opacity  : in Layer_Opacity := Opaque_Layer)
      return Group_Layer'Class is (
      Group_Layer'(Ada.Finalization.Controlled with
      Layer  => new Layer_Type'(
         Kind     => A_Surface,
         Position => Position,
         Opacity  => Opacity,
         Mode     => Mode,
         Hidden   => Hidden,
         Surface  => new Surfaces.Surface'(Surface)),
      Counter => New_Counter));

   function Layer (
      Rows     : in Positive_Row_Count;
      Cols     : in Positive_Col_Count;
      Position : in Cursor_Type   := Default_Position;
      Mode     : in Layer_Mode    := Normal;
      Hidden   : in Boolean       := False;
      Opacity  : in Layer_Opacity := Opaque_Layer)
      return Group_Layer'Class is (
      Group_Layer'(Ada.Finalization.Controlled with
      Layer  => new Layer_Type'(
         Kind     => A_Surface,
         Position => Position,
         Opacity  => Opacity,
         Mode     => Mode,
         Hidden   => Hidden,
         Surface  => new Surfaces.Surface (Rows, Cols)),
      Counter => New_Counter));

   function Layer (
      Group    : in Groups.Group;
      Position : in Cursor_Type   := Default_Position;
      Mode     : in Layer_Mode    := Normal;
      Hidden   : in Boolean       := False;
      Opacity  : in Layer_Opacity := Opaque_Layer)
      return Group_Layer'Class is (
      Group_Layer'(Ada.Finalization.Controlled with
      Layer  => new Layer_Type'(
         Kind     => A_Group,
         Position => Position,
         Opacity  => Opacity,
         Mode     => Mode,
         Hidden   => Hidden,
         Group    => Deep_Copy (Group)),
      Counter => New_Counter));

   function Move (
      Group    : in out Groups.Group;
      Position : in     Cursor_Type   := Default_Position;
      Mode     : in     Layer_Mode    := Normal;
      Hidden   : in     Boolean       := False;
      Opacity  : in     Layer_Opacity := Opaque_Layer)
      return Group_Layer'Class is (
      Layer (Group, Position, Mode, Hidden, Opacity));
   -- TODO: Use move semantics

   function No_Layer
      return Group_Layer'Class is (
      Group_Layer'(Ada.Finalization.Controlled with
      Layer   => null,
      Counter => null));

   function Empty (
      Capacity : in Layer_Index := 10)
      return Group is (
      Ada.Finalization.Controlled with
      Capacity => Capacity,
      others   => <>);

   -->> Layer Operations <<--

   function Valid (
      Object : in Group)
      return Boolean is (
      Object.Surface /= null);

   function Contains (
      Object : in Group;
      Index  : in Layer_Index)
      return Boolean is (
      Index in 1 .. Object.Capacity and then Object.Layers (Index) /= null);

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

   function Internal_Surface (
      Object : in Group;
      Index  : in Layer_Index)
      return Surface_Access is (
      (if Object.Layers (Index).Kind = A_Surface
         then Object.Layers (Index).Surface
         else Object.Layers (Index).Group.Surface));

end Malef.Groups;
