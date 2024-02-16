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

package body Malef.Counters is

   use System;
   use System.Storage_Elements;

   Counter_Bytes : constant Storage_Count
                 := Counter_Type'Size / System.Storage_Unit;

   type Integer_Access is access Integer;

   --<<------->>--
   -->> Pools <<--
   --<<------->>--

   protected body Protected_Pool is

      procedure Allocate (Address : out System.Address) is
      begin
         if First = 0 then
            if Last = Capacity then
               -- Pool exhausted
               Integer_Access'Storage_Pool.Allocate (
                  Storage_Address          => Address,
                  Size_In_Storage_Elements => Counter_And_Index'Size,
                  Alignment                => Counter_And_Index'Alignment);
               declare
                  X : Counter_And_Index with Import, Address => Address;
               begin
                  X.Dynamic := True;
               end;
            end if;
            Address := Items (Last).Counter'Address;
            Last := Last + 1;
         else
            Address := Items (First).Counter'Address;
            First := Items (First).Index;
         end if;
         Size := Size + 1;
      end Allocate;

      procedure Deallocate (Address : in System.Address) is
         Index : constant Positive
               := 1
               +  Natural ((Address - Items (Items'First)'Address)
               /  Pair_Bytes);
         X : Counter_And_Index with Import, Address => Address;
      begin
         if X.Dynamic then
            Integer_Access'Storage_Pool.Deallocate (
               Storage_Address          => Address,
               Size_In_Storage_Elements => Counter_And_Index'Size,
               Alignment                => Counter_And_Index'Alignment);
         else
            Items (Index).Index := First;
            First := Index;
            Size := Size - 1;
         end if;
      end Deallocate;

   end Protected_Pool;

   overriding
   procedure Allocate (
      Pool                     : in out Counter_Pool;
      Storage_Address          :    out Address;
      Size_In_Storage_Elements : in     Storage_Elements.Storage_Count;
      Alignment                : in     Storage_Elements.Storage_Count) is
   begin
      pragma Assert (Size_In_Storage_Elements = Counter_Bytes);
      Pool.Pool.Allocate (Storage_Address);
   end Allocate;

   overriding
   procedure Deallocate (
      Pool                     : in out Counter_Pool;
      Storage_Address          : in     Address;
      Size_In_Storage_Elements : in     Storage_Elements.Storage_Count;
      Alignment                : in     Storage_Elements.Storage_Count) is
      pragma Unreferenced (Alignment);
   begin
      pragma Assert (Size_In_Storage_Elements = Counter_Bytes);
      Pool.Pool.Deallocate (Storage_Address);
   end Deallocate;

end Malef.Counters;
