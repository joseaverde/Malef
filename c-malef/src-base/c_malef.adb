-------------------------------------------------------------------------------
--                                                                           --
--                           C _ M A L E F . A D B                           --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
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

with C_Malef.Errors;
with Malef.Exceptions;
with Malef.Wrapper;
with System;


package body C_malef is

   function Initialize return Error_Kind is
   begin

      Malef.Initialize;

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Initialization_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Initialization_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Initialize;


   function Finalize return Error_Kind is
   begin

      Malef.Finalize;

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Initialization_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Initialization_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Finalize;


   function Is_Initialized return bool is
   begin

      return (if Malef.Is_Initialized then true else false);

   end Is_Initialized;


   function Get_Height (Height : out Row_Type)
                        return Error_Kind is
   begin

      Height := Row_Type (Malef.Get_Height);

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Initialization_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Initialization_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Get_Height;


   function Get_Width (Width : out Col_Type)
                       return Error_Kind is
   begin

      Width := Col_Type (Malef.Get_Width);

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Initialization_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Initialization_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Get_Width;


   function New_Page return Error_Kind is
   begin

      Malef.New_Page;

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Initialization_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Initialization_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end New_Page;


   function Set_Title (Name : chars_ptr)
                       return Error_Kind is
   begin

      Malef.Set_Title (Value (Name));

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Initialization_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Initialization_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Set_Title;


   function Update_Terminal_Size (Is_Updated : out bool)
                                  return Error_Kind is
   begin

      Is_Updated := (if Malef.Update_Terminal_Size then true else false);

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Initialization_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Initialization_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Update_Terminal_Size;



   function Wrapper (Func    : Wrapped_Function;
                     Args    : void_ptr;
                     Ret_Val : out void_ptr)
                     return Error_Kind is
      function Malef_Wrapper is new Malef.Wrapper(void_ptr, void_ptr, Func);
   begin

      Ret_Val := Malef_Wrapper (Args);

      return No_Error;

   exception
      when Storage_Error =>
         Ret_Val := void_ptr (System.Null_Address);
         return No_Error;
      when Ada_Exception : Malef.Exceptions.Initialization_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Initialization_Error;
      when Ada_Exception : Malef.Exceptions.Bounds_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Bounds_Error;
      when Ada_Exception : Malef.Exceptions.Null_Surface_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Null_Surface_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Wrapper;

end C_malef;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
