with Ada.Text_IO;
with Interfaces.C;
with System;

package body Malef.Subsystem is

   -- Data types:
   -- https://learn.microsoft.com/en-us/windows/win32/winprog/windows-data-types

   subtype BOOL   is Interfaces.C.int;
   subtype DWORD  is Interfaces.C.unsigned_long;
   subtype SHORT  is Interfaces.C.short;
   subtype HANDLE is System.Address;

   use type DWORD;
   use type BOOL;

   type COORD is
      record
         X : SHORT;
         Y : SHORT;
      end record with
      Convention => C;

   STD_INPUT_HANDLE  : constant DWORD := DWORD'Last - 10 + 1;
   STD_OUTPUT_HANDLE : constant DWORD := DWORD'Last - 11 + 1;
   STD_ERROR_HANDLE  : constant DWORD := DWORD'Last - 12 + 1;

   ENABLE_PROCESSED_OUTPUT            : constant := 16#0001#;
   ENABLE_WRAP_AT_EOL_OUTPUT          : constant := 16#0002#;
   ENABLE_VIRTUAL_TERMINAL_PROCESSING : constant := 16#0004#;
   DISABLE_NEWLINE_AUTO_RETURN        : constant := 16#0008#;

   W_TRUE  : constant BOOL := 1;
   W_FALSE : constant BOOL := 0;

   function GetStdHandle (nStdHandle : in DWORD)
      return HANDLE with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "GetStdHandle";

   function GetConsoleMode (
      hConsoleHandle : in     HANDLE;
      lpMode         :    out DWORD)
      return BOOL with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "GetConsoleMode";

   function SetConsoleMode (
      hConsoleHandle : in HANDLE;
      lpMode         : in DWORD)
      return BOOL with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "SetConsoleMode";

   function GetLastError return DWORD with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "GetLastError";

   --<<------->>--
   -->> State <<--
   --<<------->>--

   Initialized      : Boolean := False;
   Standard_Output  : HANDLE;
   Old_Console_Mode : DWORD;

   --<<---------------->>--
   -->> Implementation <<--
   --<<---------------->>--

   procedure Initialize is
      Console_Mode : DWORD;
   begin
      if Initialized then
         return;
      end if;
      Standard_Output := GetStdHandle (STD_OUTPUT_HANDLE);
      if W_FALSE = GetConsoleMode (Standard_Output, Old_Console_Mode) then
         raise Initialization_Error with
         "Malef couldn't GetConsoleMode and thus couldn't initialize the " &
         "CMD subsystem! Error:" & GetLastError'Image;
      end if;
      Ada.Text_IO.Put_Line ("Mode = " & Old_Console_Mode'Image);
      Console_Mode := Old_Console_Mode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
      Ada.Text_IO.Put_Line ("Mode = " & Console_Mode'Image);
      -- Console_Mode := ENABLE_VIRTUAL_TERMINAL_PROCESSING
      --              or ENABLE_PROCESSED_OUTPUT
      --              or DISABLE_NEWLINE_AUTO_RETURN;
      if W_FALSE = SetConsoleMode (Standard_Output, Console_Mode) then
         Ada.Text_IO.Put_Line (Standard_Output'Image);
         Ada.Text_IO.Put_Line ("Error!");
         Ada.Text_IO.Put_Line (GetLastError'Image);
         raise Initialization_Error with
         "Malef couldn't SetConsoleMode and thus couldn't initialize the " &
         "CMD subsystem! Error:" & GetLastError'Image;
      end if;
      Initialized := True;
   end Initialize;

   procedure Finalize is
   begin
      if not Initialized then
         return;
      end if;
      if W_FALSE = SetConsoleMode (Standard_Output, Old_Console_Mode) then
         raise Initialization_Error with
         "Malef couldn't SetConsoleMode and thus couldn't initialize the " &
         "CMD subsystem! Error:" & GetLastError'Image;
      end if;
      Initialized := False;
   end Finalize;

end Malef.Subsystem;
