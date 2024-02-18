-------------------------------------------------------------------------------
--                                                                           --
--              M A L E F - P L A T F O R M - W I N 3 2 . A D S              --
--                                                                           --
--                                 M A L E F                                 --
--                                   C M D                                   --
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

with Interfaces.C;
with System;

private package Malef.Platform.Win32 with Pure is

-- Data types:
-- https://learn.microsoft.com/en-us/windows/win32/winprog/windows-data-types

   subtype BOOL   is Interfaces.C.int;
   subtype DWORD  is Interfaces.C.unsigned_long;
   subtype SHORT  is Interfaces.C.short;
   subtype WORD   is Interfaces.C.short;
   subtype UINT   is Interfaces.C.unsigned;
   subtype WCHAR  is Interfaces.C.wchar_t;
   subtype HANDLE is System.Address;

   use type DWORD;
   use type BOOL;
   use type WORD;

   Default_Rows : constant := 25;
   Default_Cols : constant := 80;

   type COORD is
      record
         X : SHORT;
         Y : SHORT;
      end record with
      Convention => C_Pass_By_Copy;

   type SMALL_RECT is
      record
         Left   : SHORT;
         Top    : SHORT;
         Right  : SHORT;
         Bottom : SHORT;
      end record with
      Convention => C;

   type CONSOLE_SCREEN_BUFFER_INFO is
      record
         dwSize               : COORD;
         dwCursorPosition     : COORD;
         wAttributes          : WORD;
         srWindow             : SMALL_RECT;
         dwMaximumWindowSize  : COORD;
      end record with
      Convention => C;

   STD_INPUT_HANDLE  : constant DWORD := DWORD'Last - 10 + 1;
   STD_OUTPUT_HANDLE : constant DWORD := DWORD'Last - 11 + 1;
   STD_ERROR_HANDLE  : constant DWORD := DWORD'Last - 12 + 1;

   ENABLE_PROCESSED_OUTPUT            : constant := 16#0001#;
   ENABLE_WRAP_AT_EOL_OUTPUT          : constant := 16#0002#;
   ENABLE_VIRTUAL_TERMINAL_PROCESSING : constant := 16#0004#;
   DISABLE_NEWLINE_AUTO_RETURN        : constant := 16#0008#;

   W_FALSE : constant BOOL := 0;
   W_TRUE  : constant BOOL := 1;

   ATTRIBUTE_ZERO           : constant := 16#0000#;
   COMMON_LVB_REVERSE_VIDEO : constant := 16#4000#;
   COMMON_LVB_UNDERSCORE    : constant := 16#8000#;

   UTF_8_CODE_PAGE : constant := 65001;

   function GetLastError
      return DWORD with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "GetLastError";

   function GetStdHandle (nStdHandle : in DWORD)
      return HANDLE with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "GetStdHandle";

end Malef.Platform.Win32;
