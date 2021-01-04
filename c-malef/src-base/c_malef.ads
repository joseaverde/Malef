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
-- This is the C thick binding of Malef.
--
-- @description
-- This is the Ada-side specification package of the Malef binding for C. It's
-- a thick binding and all the types that Malef declares at top level are
-- redeclared here. Whenever a function is called, they are quickly transformed
-- into their Ada equivalent. This way we can ensure that the C "experience"
-- doesn't get worse because it's a binding.
-- The C binding is important because the Python3 binding uses it, due to the
-- difficulties found to write the python binding in plain Ada.
--
package C_Malef is

   --============---------------============--
  ---============-- T Y P E S --============---
   --============---------------============--
   --
   -- The types declared here are the same types declared in the Malef package,
   -- but with the C convention. As this is a thick binding, I'm trying to find
   -- the most confortable way to code in C while not damaging the Ada
   -- performance and conventions. Types will have a brief description, you can
   -- read more about them at `ada-malef/src-base/malef.ads'
   --

   --
   -- Due to the lack of Error handling systems in C, we declare an enumeration
   -- type which is returned in ALMOST every function of the library, that way,
   -- we can ensure to let the user know which errors have been found and other
   -- bindings work with them.
   --
   -- @value Ada_Error
   -- This is an unhandled exception that has been raised anywhere in the code
   -- but its nature is unknown. It's called "Ada_Error" because it happens in
   -- the implementation (Ada part) and the errors don't belong to the Malef
   -- library.
   --
   -- @value No_Error
   -- This is the best kind of error, because it means there has been no error
   -- what-so-ever during the execution of a function. It's value is ZERO, so
   -- it's as simple as writing `if (operation())' to check if any exception
   -- has been raised. This is dangerous, though. Because if you do this you
   -- won't know which exception has been raised (only the information can be
   -- retrieved, not the Error_Kind).
   --
   -- @value Initialization_Error
   -- This error is raised during the initialization of any of the components
   -- of the Malef library or if the user is trying to use an object or certain
   -- functions before initialization.
   --
   -- @value Bounds_Error
   -- This error is raised every time the user is trying to access any index
   -- out of the bounds of the object.
   --
   -- @value Null_Surface_Error
   -- This error is raised when the user tries to modify a Null_Surface.
   -- Note: Null Surfaces can be used and read, but never written because they
   -- are the same constant.
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

   --
   -- This type is used for the components of the Color_Type.
   --
   type Color_Component_Type is mod 256;
   for Color_Component_Type'Size use 8;
   pragma Convention (C, Color_Component_Type);

   --
   -- This type is used to represent the indexes of the Color_Type.
   --
   -- @value R
   -- Red
   --
   -- @value G
   -- Green
   --
   -- @value B
   -- Blue
   --
   -- @value A
   -- Alpha (Opacity, 255 -> Completely opaque)
   --
   type Color_Component_Kind is (R, G, B, A);
   for Color_Component_Kind use (R => 0, G => 1, B => 2, A => 3);
   pragma Convention (C, Color_Component_Kind);

   --
   -- This type is used to represent RGBA colours with a 4 byte unsigned array
   -- of 8 bit integers.
   --
   type Color_Type is array (Color_Component_Kind'Range)
                      of     aliased Color_Component_Type;
   pragma Convention (C, Color_Type);

   --
   -- This type is used to describe different effects you can place on the
   -- text on the terminal.
   --
   -- @value Bold
   -- @value Faint
   -- @value Italic
   -- @value Underline
   -- @value Slow_Blink
   -- @value Rapid_Blink
   -- @value Reverse_Video
   -- @value Conceal
   -- @value Crossed_Out
   -- @value Doubly_Underline
   --
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

   --
   -- TODO: Change for an integer to use the `|' (or) operator. (More C-ish)
   -- This is just an array of styles telling whether they are on or off.
   --
   type Style_Array is array (Style_Type'Range) of Boolean;
   pragma Convention (C, Style_Array);

   --
   -- This is the format type, it's used to store a specific format and use it
   -- without specifying each of its elements separately.
   --
   -- @field Foreground_Color
   -- The letter's colour.
   --
   -- @field Background_Color
   -- The colour of the background.
   --
   -- @field Styles
   -- The styles that are being used by the format.
   --
   type Format_Type is
      record
         Foreground_Color : Color_Type;
         Background_Color : Color_Type;
         Styles           : Style_Array;
      end record;
   pragma Convention (C, Format_Type);

   --
   -- This is the Row_Type, as you can see, 0 is forbidden, and an exception
   -- will be raised if the value given is ZERO. It's used to cound rows.
   --
   type Row_Type is new unsigned_short
      range 1 .. unsigned_short(Malef.Row_Type'Last);
   pragma Convention (C, Row_Type);

   --
   -- Similarly to Row_Type, Col_Type counts columns and forbids the use of
   -- 0 as a valid integer.
   --
   type Col_Type is new unsigned_short
      range 1 .. unsigned_short(Malef.Col_Type'Last);
   pragma Convention (C, Col_Type);

   --
   -- This is the cursor type.
   --
   type Cursor_Type is
      record
         Row : Row_Type;
         Col : Col_Type;
      end record;
   pragma Convention (C, Cursor_Type);

   --
   -- These are each of the components of a character.
   --
   type Char_Component_Type is mod 256;
   for Char_Component_Type'Size use 8;
   pragma Convention (C, Char_Component_Type);

   --
   -- This is the character type, it's used to store unicode values.
   --
   type Char_Type is array (unsigned range 0 .. 3) of Char_Component_Type;
   pragma Convention (C, Char_Type);

   --
   -- This is the string type, it's an array of characters.
   --
   type Str_Type is array (unsigned range <>) of Char_Type;
   pragma Convention (C, Str_Type);



   --============-------------------------============--
  ---============-- O P E R A T I O N S --============---
   --============-------------------------============--


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

end C_Malef;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
