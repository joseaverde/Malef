-------------------------------------------------------------------------------
--                                                                           --
--       M A L E F - P L A T F O R M - T E R M I N A L - I N P U T -         --
--                     G E T _ I M M E D I A T E . A D B                     --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  A N S I                                  --
--                                 P O S I X                                 --
--                                                                           --
--                       A D A   S E P A R A T E   B O D Y                   --
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
with Interfaces.C;

separate (Malef.Platform.Terminal.Input)

   procedure Get_Byte_Immediate (
      Item : out Interfaces.Unsigned_8)
   is

      use Interfaces;

      type FILE is new System.Address;
      type ssize_t is new C.int;

      function ferror (
         stream : in FILE)
         return C.int with
         Import        => True,
         Convention    => C,
         External_Name => "ferror";

      function read (
         fd    : in C.int;
         buf   : in System.Address;
         count : in C.size_t)
         return ssize_t with
         Import        => True,
         Convention    => C,
         External_Name => "read";

      STDIN_FILENO : constant := 1;

      stdin : aliased constant FILE with
         Import        => True,
         Convention    => C,
         External_Name => "stdin";

      EOF_Ch : aliased constant Unsigned_8 with
         Import        => True,
         Convention    => C,
         External_Name => "__malef__platform__terminal__input___c_eof_ch";

      use type C.int;
      Buffer : aliased Unsigned_8;
      Count  : ssize_t;

   begin

      Item := 0;
      Count := read (STDIN_FILENO, Buffer'Address, 1);

      if Count = -1 then
         raise Termination_Error;
      elsif Count = 0 then
         raise Device_Error;
      elsif Buffer = EOF_Ch then
         raise End_Error;
      elsif ferror (stdin) /= 0 then
         raise Device_Error;
      else
         Item := Buffer;
      end if;

   end Get_Byte_Immediate;
