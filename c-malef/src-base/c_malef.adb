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

with Malef.Wrapper;


package body C_malef is

   procedure Initialize is
   begin

      Malef.Initialize;

   end Initialize;


   procedure Finalize is
   begin

      Malef.Finalize;

   end Finalize;


   function Is_Initialized return bool is
   begin

      return (if Malef.Is_Initialized then true else false);

   end Is_Initialized;


   function Get_Height return Row_Type is
   begin

      return Row_Type (Malef.Get_Height);

   end Get_Height;


   function Get_Width return Col_Type is
   begin

      return Col_Type (Malef.Get_Width);

   end Get_Width;


   procedure New_Page is
   begin

      Malef.New_Page;

   end New_Page;


   procedure Set_Title (Name : chars_ptr) is
   begin

      Malef.Set_Title (Value (Name));

   end Set_Title;


   function Update_Terminal_Size return bool is
   begin

      return (if Malef.Update_Terminal_Size then true else false);

   end Update_Terminal_Size;



   function Wrapper (Func : Wrapped_Function;
                     Args : void_ptr)
                     return void_ptr is
      function Malef_Wrapper is new Malef.Wrapper(void_ptr, void_ptr, Func);
   begin

      return Malef_Wrapper (Args);

   end Wrapper;

end C_malef;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
