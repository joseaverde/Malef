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
with System.Atomic_Operations.Integer_Arithmetic;

with Ada.Text_IO;

package body Malef.Groups is

   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Layer_Type,
      Name   => Layer_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Control_Type,
      Name   => Control_Access);

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
      Atomic_Type => Atomic_Counter);

   function Steal (
      Object : in Group_Element'Class)
      return Layer_Access is
   begin
      if Object.Control = null then
         return null;
      end if;
      if Test_And_Set.Atomic_Test_And_Set (Object.Control.Owned) then
         raise Program_Error with
         "Group_Element has already been added to a Group which obtained " &
         "ownership of the Element. Refrain from using Group_Element's. "  &
         "Their purpose is implementing Aggregates for Groups.";
      end if;
      return Object.Layer;
   end Steal;

   overriding
   procedure Adjust (
      Object : in out Group_Element) is
   begin
      if Object.Control = null then
         return;
      end if;
      Atomic_Counters.Atomic_Add (Object.Control.Counter, 1);
   end Adjust;

   overriding
   procedure Finalize (
      Object : in out Group_Element)
   is
      use all type Atomic_Boolean;
   begin
      if Object.Control = null then
         return;
      end if;
      if Atomic_Counters.Atomic_Fetch_And_Subtract (Object.Control.Counter, 1)
         = 1
      then
         if Object.Control.Owned = 0 then
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
                     Free (Group);
                  end;
            end case;
            Free (Object.Layer);
         end if;
         Free (Object.Control);
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

   -->> As a Composer <<--

   procedure Update (
      Object : in out Group) is
   begin
      null;
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

   procedure Clear (
      Object : in out Group) is
   begin
      for I in Object.Layers'Range when Contains (Object, I) loop
         Delete (Object, I);
      end loop;
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

   -->> As an Aggregate <<--

   procedure Add_Unnamed (
      Object   : in out Group;
      New_Item : in     Group_Element'Class) is
   begin
      Object.Index := @ + 1;
      Object.Count := (if New_Item.Layer = null then @ else @ + 1);
      Object.Layers (Object.Index) := Steal (New_Item);
      Object.Updated := True;
   end Add_Unnamed;

   procedure Assign_Indexed (
      Object   : in out Group;
      Index    : in     Layer_Index;
      New_Item : in     Group_Element'Class) is
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
         Object.Count := @ + 1;
         Object.Layers (Index) := Steal (New_Item);
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

end Malef.Groups;
