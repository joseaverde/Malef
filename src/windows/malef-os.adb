-------------------------------------------------------------------------------
--                                                                           --
--                          M A L E F - O S . A D B                          --
--                               ( L I N U X )                               --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   B O D Y                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2024 José Antonio Verde Jiménez  All Rights Reserved  --
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

with Interfaces.C;
with Malef.Events;

package body Malef.OS is

   procedure C_Dimensions (
      Rows : out Interfaces.C.short;
      Cols : out Interfaces.C.short) with
      Import        => True,
      Convention    => C,
      External_Name => "_malef_getConsoleScreenSize";

   procedure Get_Dimensions (
      Rows : out Positive_Row_Count;
      Cols : out Positive_Col_Count) is
   begin
      C_Dimensions (Interfaces.C.short (Rows), Interfaces.C.short (Cols));
   end Get_Dimensions;

   -->> Initialize <<--

   procedure Initialize is
   begin
      Malef.Events.Event_Handler.Update_Terminal_Size;
   end Initialize;

   -->> Finalize <<--

   procedure Finalize is null;

   -->> Prepare <<--

   procedure C_Prepare with
      Import        => True,
      Convention    => C,
      External_Name => "_malef_setupConsole";

   procedure Prepare is
   begin
      C_Prepare;
   end Prepare;

   -->> Restore <<--

   procedure C_Restore with
      Import        => True,
      Convention    => C,
      External_Name => "_malef_restoreConsole";

   procedure Restore is
   begin
      C_Restore;
   end Restore;

end Malef.OS;
