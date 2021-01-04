-------------------------------------------------------------------------------
--                                                                           --
--                           C _ M A L E F . A D S                           --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
--                                                                           --
-------------------------------------------------------------------------------
--     Copyright (c) 2021 José Antonio Verde Jiménez All Rights Reserved     --
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

with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with Interfaces.C.Strings;    use Interfaces.C.Strings;
with Malef;

--
-- @summary
--
--
-- @description
--
package C_malef is

   type Error_Kind is (Ada_Error,
                       No_Error,
                       Initialization_Error,
                       Bounds_Error,
                       Null_Surface_Error);
   for Error_Kind use
      (Ada_Error            => -1,
       No_Error             =>  0,
       Initialization_Error =>  1,
       Bounds_Error         =>  2,
       Null_Surface_Error   =>  3);
   pragma Convention (C, Error_Kind);

   type Color_Component_Type is mod 256;
   for Color_Component_Type'Size use 8;
   pragma Convention (C, Color_Component_Type);

   type Color_Component_Kind is (R, G, B, A);
   for Color_Component_Kind use (R => 0, G => 1, B => 2, A => 3);
   pragma Convention (C, Color_Component_Kind);

   type Color_Type is array (Color_Component_Kind'Range)
                      of     aliased Color_Component_Type;
   pragma Convention (C, Color_Type);

   type Style_Type is (Bold,        Faint,          Italic,        Underline,
                       Slow_Blink,  Rapid_Blink,    Reverse_Video, Conceal,
                       Crossed_Out, Doubly_Underline);
   for Style_Type use (
      Bold             => 0,
      Faint            => 1,
      Italic           => 2,
      Underline        => 3,
      Slow_Blink       => 4,
      Rapid_Blink      => 5,
      Reverse_Video    => 6,
      Conceal          => 7,
      Crossed_Out      => 8,
      Doubly_Underline => 9);
   pragma Convention (C, Style_Type);

   type Style_Array is array (Style_Type'Range) of Boolean;
   pragma Convention (C, Style_Array);

   type Format_Type is
      record
         Foreground_Color : Color_Type;
         Background_Color : Color_Type;
         Styles           : Style_Array;
      end record;
   pragma Convention (C, Format_Type);

   -- We allow 0 for unknown ranges.
   type Row_Type is new unsigned_short
      range 0 .. unsigned_short(Malef.Row_Type'Last);
   pragma Convention (C, Row_Type);

   type Col_Type is new unsigned_short
      range 0 .. unsigned_short(Malef.Col_Type'Last);
   pragma Convention (C, Col_Type);

   type Cursor_Type is
      record
         Row : Row_Type;
         Col : Col_Type;
      end record;
   pragma Convention (C, Cursor_Type);

   type Char_Component_Type is mod 256;
   for Char_Component_Type'Size use 8;
   pragma Convention (C, Char_Component_Type);

   type Char_Type is array (unsigned range 0 .. 3) of Char_Component_Type;
   pragma Convention (C, Char_Type);

   type Str_Type is array (unsigned range <>) of Char_Type;
   pragma Convention (C, Str_Type);

   function Initialize return Error_Kind;
   pragma Export (C, Initialize, "malef_initialize");

   function Finalize return Error_Kind;
   pragma Export (C, Finalize, "malef_finalize");

   function Is_Initialized return bool;
   pragma Export (C, Is_Initialized, "malef_isInitialized");

   function Get_Height (Height : out Row_Type)
                        return Error_Kind;
   pragma Export (C, Get_Height, "malef_getHeight");

   function Get_Width (Width : out Col_Type)
                       return Error_Kind;
   pragma Export (C, Get_Width, "malef_getWidth");

   function New_Page return Error_Kind;
   pragma Export (C, New_Page, "malef_newPage");

   function Set_Title (Name : chars_ptr)
                       return Error_Kind;
   pragma Export (C, Set_Title, "malef_setTitle");

   function Update_Terminal_Size (Is_Updated : out bool)
                                  return Error_Kind;
   pragma Export (C, Update_Terminal_Size, "malef_updateTerminalSize");

   type Wrapped_Function is access function (Args: void_ptr) return void_ptr;
   pragma Convention (C, Wrapped_Function);
   function Wrapper (Func    : Wrapped_Function;
                     Args    : void_ptr;
                     Ret_Val : out void_ptr)
                     return Error_Kind;
   pragma Export (C, Wrapper, "malef_wrapper");

end C_malef;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
