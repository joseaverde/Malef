-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - G R O U P S . A D B                      --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   B O D Y                              --
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

with Ada.Unchecked_Deallocation;
with Malef.Groups.Composers;
with System.Atomic_Operations.Integer_Arithmetic;
with System.Atomic_Operations.Test_And_Set;

package body Malef.Groups is

   use System.Atomic_Operations;
   use type Counters.Atomic_Flag;
   use type Counters.Atomic_Counter;
   use type Counters.Counter_Access;

   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Layer_Type,
      Name   => Layer_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Counters.Counter_Type,
      Name   => Counters.Counter_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Surfaces.Surface,
      Name   => Surface_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Group,
      Name   => Group_Access);

   --<<---------------->>--
   -->> Group Elements <<--
   --<<---------------->>--

   package Atomic_Counters is
      new System.Atomic_Operations.Integer_Arithmetic (
      Atomic_Type => Counters.Atomic_Counter);

   function Steal (
      Object : in Group_Layer'Class)
      return Layer_Access is
   begin
      if Object.Counter = null then
         return null;
      end if;
      if Test_And_Set.Atomic_Test_And_Set (Object.Counter.Locked) then
         raise Program_Error with
         "Group_Layer has already been added to a Group which obtained " &
         "ownership of the Element. Refrain from using Group_Layer's. "  &
         "Their purpose is implementing Aggregates for Groups.";
      end if;
      return Object.Layer;
   end Steal;

   overriding
   procedure Adjust (
      Object : in out Group_Layer) is
   begin
      if Object.Counter = null then
         return;
      end if;
      Atomic_Counters.Atomic_Add (Object.Counter.Counter, 1);
   end Adjust;

   overriding
   procedure Finalize (
      Object : in out Group_Layer)
   is
      use all type Counters.Atomic_Flag;
   begin
      if Object.Counter = null then
         return;
      end if;
      if Atomic_Counters.Atomic_Fetch_And_Subtract (Object.Counter.Counter, 1)
         = 1
      then
         if Object.Counter.Locked = 0 then
            case Object.Layer.Kind is
               when A_Surface =>
                  declare
                     Surface : Surface_Access := Object.Layer.Surface;
                  begin
                     Free (Surface);
                  end;
               when A_Group =>
                  declare
                     Group : Group_Access := Object.Layer.Group;
                  begin
                     Group.all.Clear;
                     Free (Group);
                  end;
            end case;
            Free (Object.Layer);
         end if;
         Free (Object.Counter);
      end if;
   end Finalize;

   --<<-------->>--
   -->> Groups <<--
   --<<-------->>--

   procedure Set_Updated (
      Object : in out Group;
      Layer  : in     Layer_Index)
   is
      pragma Unreferenced (Layer);
   begin
      -- TODO: Optimise
      Object.Updated := True;
   end Set_Updated;

   -- procedure Lock (
   --    Object : in out Group) is
   -- begin
   --    if Test_And_Set.Atomic_Test_And_Set (Object.Counter.Locked) then

   -->> As a Composer <<--

   function Increase_Size (
      Object : in out Group;
      Offset :    out Cursor_Type)
      return Boolean
   is
      Min : Cursor_Type := (Row_Type'Last, Col_Type'Last);
      Max : Cursor_Type := (Row_Type'First, Col_Type'First);
      Pos : Cursor_Type;
      Siz : Cursor_Type;
   begin

      -- Get the sizes of the objects and their relative position. And obtain
      -- the extensions of the new Group.

      for Index in Object.Layers'Range when Object.Layers (Index) /= null loop
         Pos := Get_Position (Object, Index);
         Siz := (Row => Internal_Surface (Object, Index).Rows,
                 Col => Internal_Surface (Object, Index).Cols);
         Min := Cursor_Type'(Row => Row_Type'Min (Min.Row, Pos.Row),
                             Col => Col_Type'Min (Min.Col, Pos.Col));
         Max := Cursor_Type'(Row => Row_Type'Max (Max.Row, Pos.Row + Siz.Row),
                             Col => Col_Type'Max (Max.Col, Pos.Col + Siz.Col));
      end loop;

      -- If there are no layers, exit.

      if Max.Col < Min.Col or else Max.Row < Min.Row then
         return False;
      end if;

      -- Otherwise check if size has to be increased.

      Object.Region := Min;
      Offset := (-Min.Row, -Min.Col);
      Siz := (Max.Row - Min.Row, Max.Col - Min.Col);

      if Object.Surface.Rows < Siz.Row or else Object.Surface.Cols < Siz.Col
      then
         Free (Object.Surface);
         Object.Surface := new Surfaces.Surface (Siz.Row, Siz.Col);
      end if;

      return True;

   end Increase_Size;

   procedure Clear_Internal_Surface (
      Object : in out Group) is
   begin
      Object.Surface.Fill ((0, 0, 0, 0), (0, 0, 0, 0), Nul, No_Style);
   end Clear_Internal_Surface;

   function Mark_Updated (
      Object : in out Group)
      return Boolean
   is
      Count : Natural := 0;
   begin
      -- TODO: Add optimisation to avoid redrawing a lot.
      for Layer of Object.Layers when Layer /= null loop
         if Layer.Kind = A_Surface and then Layer.Surface.Modified then
            Layer.Surface.Set_Up_to_Date;
            Count := Count + 1;
         elsif Layer.Kind = A_Group and then Layer.Group.Updated then
            Layer.Group.Update;
            Count := Count + 1;
         end if;
      end loop;
      return Count > 0;
   end Mark_Updated;

   procedure Update (
      Object : in out Group)
   is
      Mode   : Layer_Mode := Normal;
      Offset : Cursor_Type;
   begin
      -- TODO: Set tampering detector
      if not Mark_Updated (Object) or else not Increase_Size (Object, Offset)
      then
         return;
      end if;
      Clear_Internal_Surface (Object);
      for Index in reverse Object.Layers'Range
         when Object.Layers (Index) /= null
      loop
         Composers.Get_Composer (Mode).all (Object, Index, Offset);
         Mode := Get_Mode (Object, Index);
      end loop;
      Object.Updated := True;
   end Update;

   -->> Put Image <<--

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Group) is
   begin
      Buffer.Put ("GROUP ["); Buffer.Put (Arg.Count'Image);
      Buffer.Put ("/");       Buffer.Put (Arg.Capacity'Image);
      Buffer.Put (" ]");
      Buffer.Wide_Wide_Put (Arg.Surface.all'Wide_Wide_Image);
   end Put_Image;

   -->> As a Container <<--

   function Set_Surface (
      Object : aliased in out Group;
      Index  :         in     Layer_Index)
      return Surface_Reference_Type is
   begin
      Object.Updated := True;
      return Surface_Reference_Type'(Element => Object.Layers (Index).Surface);
   end Set_Surface;

   function Set_Group (
      Object : aliased in out Group;
      Index  :         in     Layer_Index)
      return Group_Reference_Type is
   begin
      Object.Updated := True;
      return Group_Reference_Type'(Element => Object.Layers (Index).Group);
   end Set_Group;

   procedure Clear (
      Object : in out Group) is
   begin
      for I in Object.Layers'Range when Contains (Object, I) loop
         Delete (Object, I);
      end loop;
      Object.Region := (0, 0);
   end Clear;

   procedure Delete (
      Object : in out Group;
      Index  : in     Layer_Index) is
   begin
      case Object.Layers (Index).Kind is
         when A_Surface =>
            declare
               Surface : Surface_Access := Object.Layers (Index).Surface;
            begin
               Free (Surface);
            end;
         when A_Group =>
            declare
               Group : Group_Access := Object.Layers (Index).Group;
            begin
               Clear (Group.all);
               Free (Group);
            end;
      end case;
      Free (Object.Layers (Index));
      Object.Updated := True;
      Object.Count := @ - 1;
   end Delete;

   -- function Move_Out_Group (
   --    Object : in out Group)
   --    return Group_Layer'Class is
   -- begin

   procedure Insert (
      Object   : in out Group;
      Index    : in     Layer_Index;
      Surface  : in     Surfaces.Surface;
      Position : in     Cursor_Type   := Default_Position;
      Mode     : in     Layer_Mode    := Normal;
      Hidden   : in     Boolean       := False;
      Opacity  : in     Layer_Opacity := Opaque_Layer) is
   begin
      Object.Assign_Indexed (Index    => Index,
                             New_Item => Layer (Surface  => Surface,
                                                Position => Position,
                                                Mode     => Mode,
                                                Hidden   => Hidden,
                                                Opacity  => Opacity));
      Set_Updated (Object, Index);
   end Insert;

   procedure Insert (
      Object   : in out Group;
      Index    : in     Layer_Index;
      Group    : in     Groups.Group;
      Position : in     Cursor_Type   := Default_Position;
      Mode     : in     Layer_Mode    := Normal;
      Hidden   : in     Boolean       := False;
      Opacity  : in     Layer_Opacity := Opaque_Layer) is
   begin
      Object.Assign_Indexed (Index    => Index,
                             New_Item => Layer (Group    => Group,
                                                Position => Position,
                                                Mode     => Mode,
                                                Hidden   => Hidden,
                                                Opacity  => Opacity));
      Set_Updated (Object, Index);
   end Insert;

   -->> As an Aggregate <<--

   procedure Add_Unnamed (
      Object   : in out Group;
      New_Item : in     Group_Layer'Class) is
   begin
      Object.Index := @ + 1;
      Object.Layers (Object.Index) := Steal (New_Item);
      Object.Count := (if New_Item.Layer = null then @ else @ + 1);
      Object.Updated := True;
      -- TODO: Use Set_Updated function
   end Add_Unnamed;

   procedure Assign_Indexed (
      Object   : in out Group;
      Index    : in     Layer_Index;
      New_Item : in     Group_Layer'Class) is
   begin
      if not Contains (Object, Index) and then not Contains (New_Item) then
         null;
      elsif not Contains (Object, Index) then
         Object.Count := @ + 1;
         Object.Layers (Index) := Steal (New_Item);
      elsif not Contains (New_Item) then
         Delete (Object, Index);
      else
         Delete (Object, Index);
         Object.Layers (Index) := Steal (New_Item);
         Object.Count := @ + 1;
      end if;
      Object.Updated := True;
   end Assign_Indexed;

   -->> Layer Operations <<--

   procedure Hide (
      Object : in out Group;
      Index  : in     Layer_Index) is
   begin
      if not Object.Layers (Index).Hidden then
         Object.Layers (Index).Hidden := True;
         Set_Updated (Object, Index);
      end if;
   end Hide;

   procedure Show (
      Object : in out Group;
      Index  : in     Layer_Index) is
   begin
      if Object.Layers (Index).Hidden then
         Object.Layers (Index).Hidden := False;
         Set_Updated (Object, Index);
      end if;
   end Show;

   procedure Set_Mode (
      Object : in out Group;
      Index  : in     Layer_Index;
      Item   : in     Layer_Mode) is
   begin
      if Item = Object.Layers (Index).Mode then
         return;
      end if;
      Object.Layers (Index).Mode := Item;
      Set_Updated (Object, Index);
   end Set_Mode;

   procedure Set_Opacity (
      Object : in out Group;
      Index  : in     Layer_Index;
      Item   : in     Layer_Opacity) is
   begin
      if Item = Object.Layers (Index).Opacity then
         return;
      end if;
      Object.Layers (Index).Opacity := Item;
      Set_Updated (Object, Index);
   end Set_Opacity;

   procedure Move_Layer (
      Object : in out Group;
      Index  : in     Layer_Index;
      To     : in     Cursor_Type) is
   begin
      if To = Object.Layers (Index).Position then
         return;
      end if;
      Set_Updated (Object, Index);
      Object.Layers (Index).Position := To;
      Set_Updated (Object, Index);
   end Move_Layer;

   -->> Layer Operations <<--

   procedure Reference (
      Object : in out Group) is
   begin
      Atomic_Counters.Atomic_Add (Object.Counter.Counter, 1);
   end Reference;

   function Unreference (
      Object : in out Group)
      return Boolean is
      use Atomic_Counters;
   begin
      return Atomic_Fetch_And_Subtract (Object.Counter.Counter, 1) = 1;
   end Unreference;

   overriding
   procedure Adjust (
      Object : in out Group) is
   begin
      Reference (Object);
   end Adjust;

   overriding
   procedure Finalize (
      Object : in out Group) is
   begin
      if Unreference (Object) then
         Object.Clear;
         Free (Object.Surface);
         Free (Object.Counter);
      end if;
   end Finalize;

end Malef.Groups;
