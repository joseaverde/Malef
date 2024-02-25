-------------------------------------------------------------------------------
--                                                                           --
--    M A L E F - P L A T F O R M - T E R M I N A L - O U T P U T . A D B    --
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

with Ada.Characters.Conversions;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Malef.Debug;
with Malef.Palettes;
with Malef.Platform.Generic_Buffer;
with Malef.Platform.Win32;

package body Malef.Platform.Terminal.Output is

   use Win32;
   use type HANDLE;
   use type BOOL;
   use type SHORT;

   Palette_Index_As_WORD : constant array (Palette_Index) of WORD := [
       0 => 2#0000#,  1 => 2#0100#,  2 => 2#0010#,  3 => 2#0110#,
       4 => 2#0001#,  5 => 2#0101#,  6 => 2#0011#,  7 => 2#0111#,
       8 => 2#1000#,  9 => 2#1100#, 10 => 2#1010#, 11 => 2#1110#,
      12 => 2#1001#, 13 => 2#1101#, 14 => 2#1011#, 15 => 2#1111#];

   package Buffer is
      new Platform.Generic_Buffer (
      Capacity => 256,
      Stream   => Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output)
      );

   Standard_Output  : HANDLE;
   Old_Console_Mode : DWORD;
   Last_Format      : WORD;
   Position         : Cursor_Type;

   Palette : constant Palettes.Palette_Type := [
       0 => [  0,   0,   0, 255],
       1 => [128,   0,   0, 255],
       2 => [  0, 128,   0, 255],
       3 => [128, 128,   0, 255],
       4 => [  0,   0,   0, 255],
       5 => [128,   0, 128, 255],
       6 => [  0, 128, 128, 255],
       7 => [192, 192, 192, 255],
       8 => [128, 128, 128, 255],
       9 => [255,   0,   0, 255],
      10 => [  0, 255,   0, 255],
      11 => [255, 255,   0, 255],
      12 => [  0, 255, 255, 255],
      13 => [255,   0, 255, 255],
      14 => [  0, 255, 255, 255],
      15 => [255, 255, 255, 255]];

   --<<---------->>--
   -->> External <<--
   --<<---------->>--

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

   function SetConsoleOutputCP (
      wCodePageID : in UINT)
      return BOOL with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "SetConsoleOutputCP";

   --<<---------------->>--
   -->> Initialization <<--
   --<<---------------->>--

   procedure Initialize is
      Console_Mode : DWORD;
   begin
      Standard_Output := GetStdHandle (STD_OUTPUT_HANDLE);
      Last_Format := 0;
      Position := (1, 1);
      if W_FALSE = GetConsoleMode (Standard_Output, Old_Console_Mode) then
         raise Initialization_Error with
         "Malef couldn't GetConsoleMode and thus couldn't initialize the " &
         "CMD subsystem! Error:" & GetLastError'Image;
      end if;
      -- TODO: ENABLE_VIRTUAL_TERMINAL_PROCESSING only works on newer CMDs.
      Console_Mode := Old_Console_Mode;
      -- Console_Mode := ENABLE_VIRTUAL_TERMINAL_PROCESSING
      --              or ENABLE_PROCESSED_OUTPUT
      --              or DISABLE_NEWLINE_AUTO_RETURN;
      if W_FALSE = SetConsoleMode (Standard_Output, Console_Mode) then
         raise Initialization_Error with
         "Malef couldn't SetConsoleMode and thus couldn't initialize the " &
         "CMD subsystem! Error:" & GetLastError'Image;
      end if;
      if W_FALSE = SetConsoleOutputCP (UTF_8_CODE_PAGE) then
         raise Initialization_Error with
         "Couldn't set codepage to 65001 for UTF-8";
      end if;
   end Initialize;

   procedure Set_Attribute (
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type);

   procedure Finalize is
   begin
      Buffer.Flush;
      Set_Attribute (0, 7, [others => False]);
      if W_FALSE = SetConsoleMode (Standard_Output, Old_Console_Mode) then
         raise Initialization_Error with
         "Malef couldn't SetConsoleMode and thus couldn't initialize the " &
         "CMD subsystem! Error:" & GetLastError'Image;
      end if;
   end Finalize;

   --<<-------->>--
   -->> Output <<--
   --<<-------->>--

   -->> Helpers <<--

   procedure Move_To (
      Cursor : in Cursor_Type)
   is
      function SetConsoleCursorPosition (
         hConsoleOutput   : in HANDLE;
         dwCursorPosition : in COORD)
         return BOOL with
         Import        => True,
         Convention    => Stdcall,
         External_Name => "SetConsoleCursorPosition";
      Coordinates : constant COORD := (Y => SHORT (Cursor.Row),
                                       X => SHORT (Cursor.Col));
      Dummy : BOOL;
   begin
      if Position /= Cursor then
         Flush;
         Dummy := SetConsoleCursorPosition (Standard_Output, Coordinates);
         Position := Cursor;
      end if;
   end Move_To;

   procedure Set_Attribute (
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type)
   is
      function SetConsoleTextAttribute (
         hConsoleOutput : in HANDLE;
         wAttributes    : in WORD)
         return BOOL with
         Import        => True,
         Convention    => Stdcall,
         External_Name => "SetConsoleTextAttribute";
      Format : constant WORD :=  (Palette_Index_As_WORD (Foreground)
                                + Palette_Index_As_WORD (Background) * 16
                                + (if Style (Reverse_Video)
                                    then COMMON_LVB_REVERSE_VIDEO
                                    else ATTRIBUTE_ZERO)
                                + (if       Style (Bold)
                                    or else Style (Italic)
                                    or else Style (Underline)
                                       then COMMON_LVB_UNDERSCORE
                                       else ATTRIBUTE_ZERO));
   begin
      if Format /= Last_Format then
         Flush;
         if W_FALSE = SetConsoleTextAttribute (Standard_Output, Format) then
            Debug.Put (Debug.Types.Critical,
                       "SetConsoleTextAttribute failed with erro code:"
                       & GetLastError'Image);
         end if;
         Last_Format := Format;
      end if;
   end Set_Attribute;

   procedure Set_Attribute (
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      Set_Attribute (
         Background => Palettes.Nearest (Palette, Background),
         Foreground => Palettes.Nearest (Palette, Foreground),
         Style      => Style);
   end Set_Attribute;

   -->> Implementation <<--

   procedure Begin_Frame is
   begin
      Flush;
   end Begin_Frame;

   procedure End_Frame is
   begin
      Flush;
   end End_Frame;

   procedure Put (
      Position   : in Cursor_Type;
      Item       : in Glyph_String;
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      Move_To (Position);
      Set_Attribute (Background, Foreground, Style);
      Buffer.Wide_Wide_Put (Item);
      Output.Position.Col := @ + Item'Length;
   end Put;

   procedure Put_Indexed (
      Position   : in Cursor_Type;
      Item       : in Glyph_String;
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type) is
   begin
      Move_To (Position);
      Set_Attribute (Background, Foreground, Style);
      Buffer.Wide_Wide_Put (Item);
      Output.Position.Col := @ + Item'Length;
   end Put_Indexed;

   procedure Put (
      Position   : in Cursor_Type;
      Item       : in Glyph;
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      Move_To (Position);
      Set_Attribute (Background, Foreground, Style);
      Buffer.Wide_Wide_Put (Item);
      Output.Position.Col := @ + 1;
   end Put;

   procedure Put_Indexed (
      Position   : in Cursor_Type;
      Item       : in Glyph;
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type) is
   begin
      Move_To (Position);
      Set_Attribute (Background, Foreground, Style);
      Buffer.Wide_Wide_Put (Item);
      Output.Position.Col := @ + 1;
   end Put_Indexed;

   procedure Flush is
   begin
      Buffer.Flush;
   end Flush;

   procedure Set_Title (
      Item : in Wide_Wide_String)
   is
      -- TODO: Check if it uses UTF-16 or just 16 bits per character
      use Ada.Characters.Conversions;
      function SetConsoleTitle (
         lpConsoleTitle : not null access constant WCHAR)
         return BOOL with
         Import        => True,
         Convention    => Stdcall,
         External_Name => "SetConsoleTitleW";
      Title : array (1 .. Item'Length + 1) of aliased WCHAR
            := [for I in 1 .. Item'Length =>
                  WCHAR'Val (Wide_Character'Pos (
                     To_Wide_Character (Item (I - Item'First + 1))))]
               & WCHAR'First;
   begin
      if W_FALSE = SetConsoleTitle (Title (Title'First)'Access) then
         Debug.Put (Debug.Types.Critical,
                    "SetConsoleTitleW failed with error:"
                    & GetLastError'Image);
      end if;
   end Set_Title;

end Malef.Platform.Terminal.Output;
