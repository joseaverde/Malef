-------------------------------------------------------------------------------
--                                                                           --
-- MALEF - S Y S T E M S - D Y N A M I C _ L I B R A R Y _ L O A D E R . ADB --
--                               ( L I N U X )                               --
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

with Interfaces.C;
with Interfaces.C.Strings;
with Malef.Exceptions;

package body Malef.Systems.Dynamic_Library_Loader is

   function dlerror return Interfaces.C.Strings.Chars_Ptr
      with Import        => True,
           Convention    => C,
           External_Name => "dlerror";

   function dlopen (library_name : String;
                    mode         : Interfaces.C.int)
                    return Library_Handle
      with Import        => True,
           Convention    => C,
           External_Name => "dlopen";

   function dlclose (handle : Library_Handle)
                     return Interfaces.C.int
      with Import        => True,
           Convention    => C,
           External_Name => "dlclose";

   RTLD_LAZY : constant := 1;

   function Get_Library_Prefix return String is
   begin

      return "lib";

   end Get_Library_Prefix;


   function Get_Library_Suffix return String is
   begin

      return "so";

   end Get_Library_Suffix;


   function Load_Library (
      Path : String)
      return Library_Handle
   is
      Handle : Library_Handle;
   begin

      Handle := dlopen (Path & ASCII.Nul, RTLD_LAZY);

      if Handle = Library_Handle (System.Null_Address) then
         raise Malef.Exceptions.Initialization_Error
         with Interfaces.C.Strings.Value (dlerror);
      end if;


      return Handle;

   end Load_Library;


   procedure Unload_Library (Handle : in out Library_Handle) is
      Dummy : Interfaces.C.int
         with Unreferenced;
   begin

      Dummy := dlclose (Handle);
      Handle := Library_Handle (System.Null_Address);

   end Unload_Library;

end Malef.Systems.Dynamic_Library_Loader;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
