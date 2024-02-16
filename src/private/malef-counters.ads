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

with System;
with System.Atomic_Operations.Test_And_Set;
with System.Storage_Pools;
with System.Storage_Elements;

private with Malef_Config;

private package Malef.Counters with Preelaborate is

   --<<------->>--
   -->> Pools <<--
   --<<------->>--

   type Counter_Pool (Capacity : Positive) is
      new System.Storage_Pools.Root_Storage_Pool with
      private;

   overriding
   procedure Allocate (
      Pool                     : in out Counter_Pool;
      Storage_Address          :    out System.Address;
      Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
      Alignment                : in     System.Storage_Elements.Storage_Count);

   overriding
   procedure Deallocate (
      Pool                     : in out Counter_Pool;
      Storage_Address          : in     System.Address;
      Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
      Alignment                : in     System.Storage_Elements.Storage_Count);

   overriding
   function Storage_Size (
      Pool : in Counter_Pool)
      return System.Storage_Elements.Storage_Count;

   --<<---------->>--
   -->> Counters <<--
   --<<---------->>--

   subtype Atomic_Flag is
      System.Atomic_Operations.Test_And_Set.Test_And_Set_Flag;

   type Atomic_Counter is new Integer with Atomic;

   type Counter_Type is
      record
         Counter : aliased Atomic_Counter := 1;
         Locked  : aliased Atomic_Flag    := 0;
      end record;

   type Counter_Access is access all Counter_Type;

private

   Pair_Bits : constant := 128;

   -- 32 bits are unused, but it is inteded to be like that. For alignment
   -- reasons

   type Counter_And_Index is
      record
         Counter : aliased Counter_Type;
         Index   : aliased Natural;
         Dynamic : Boolean;
      end record with
      Object_Size => Pair_Bits;

   type Counter_And_Index_Array is
      array (Positive range <>)
      of aliased Counter_And_Index with
      Alignment => Pair_Bits;

   protected type Protected_Pool (Capacity : Positive) is
      procedure Allocate (Address : out System.Address);
      procedure Deallocate (Address : in System.Address);
   private
      Size  : Natural  := 0;
      First : Natural  := 0;
      Last  : Positive := 1;
      Items : Counter_And_Index_Array (1 .. Capacity);
   end Protected_Pool;

   type Counter_Pool (Capacity : Positive) is
      new System.Storage_Pools.Root_Storage_Pool with
      record
         Pool : Protected_Pool (Capacity);
      end record;

   Pool : Counter_Pool (Malef_Config.Max_Controlled_Pool_Capacity);

   for Counter_Access'Storage_Pool use Pool;

   --<<------->>--
   -->> Pools <<--
   --<<------->>--

   Pair_Bytes : constant := Pair_Bits / System.Storage_Unit;

   overriding
   function Storage_Size (
      Pool : in Counter_Pool)
      return System.Storage_Elements.Storage_Count is (
      System.Storage_Elements.Storage_Count (Pool.Capacity * Pair_Bytes));

end Malef.Counters;
