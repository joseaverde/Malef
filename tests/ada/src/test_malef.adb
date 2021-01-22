-------------------------------------------------------------------------------
--                                                                           --
--                        T E S T _ M A L E F . A D B                        --
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

with Ada.Text_IO;
with Malef;
with Malef.Exceptions;
with Tests;


package body Test_Malef is

   function Initialize return String is
   begin

      Malef.Initialize;
      return "<>";

   exception
      when Malef.Exceptions.Initialization_Error =>
         return "Initialization_Error";
   end Initialize;


   function Finalize return String is
   begin

      Malef.Finalize;
      return "<>";

   exception
      when Malef.Exceptions.Initialization_Error =>
         return "Initialization_Error";
   end Finalize;


   function Is_Initialized return String is
   begin

      return (if Malef.Is_Initialized then "True" else "False");

   end Is_Initialized;


   function To_String (I : Integer) return String with Inline is
      S : constant String := I'Image;
      N : Positive        := S'First;
   begin

      while S(N) = ' ' loop
         N := N + 1;
      end loop;

      return S(N .. S'Last);

   end To_String;


   function Get_Height return String is
   begin

      if Malef.Is_Initialized then
         Tests.Wait ("Set the terminal height to 20, and press any key to " &
                     "continue...");
         if Malef.Update_Terminal_Size then
            return "$No automatic update";
         end if;
      end if;

      return '@' & To_String (Integer(Malef.Get_Height));

   exception
      when Malef.Exceptions.Initialization_Error =>
         return "Initialization_Error";
   end Get_Height;


   function Get_Width return String is
   begin

      if Malef.Is_Initialized then
         Tests.Wait ("Set the terminal width to 50, and press any key to " &
                     "continue...");
         if Malef.Update_Terminal_Size then
            return "$No automatic update";
         end if;
      end if;

      return '@' & To_String (Integer(Malef.Get_Width));

   exception
      when Malef.Exceptions.Initialization_Error =>
         return "Initialization_Error";
   end Get_Width;


   function New_Page return String is
   begin

      Malef.New_Page;

      return "<>";

   exception
      when Malef.Exceptions.Initialization_Error =>
         return "Initialization_Error";
   end New_Page;


   function Set_Title return String is
   begin

      Malef.Set_Title("Testing Malef!");

      return "<>";

   exception
      when Malef.Exceptions.Initialization_Error =>
         return "Initialization_Error";
   end Set_Title;



   procedure Main is
   begin
      
      Tests.Test (Unit     => Initialize'Access,
                  Name     => "Initialize",
                  Expected => "<>");

      Tests.Test (Unit     => Initialize'Access,
                  Name     => "Initialize",
                  Expected => "Initialization_Error");

      Tests.Test (Unit     => Finalize'Access,
                  Name     => "Finalize",
                  Expected => "<>");

      Tests.Test (Unit     => Finalize'Access,
                  Name     => "Finalize",
                  Expected => "Initialization_Error");

      Tests.Test (Unit     => Get_Height'Access,
                  Name     => "Get_Height",
                  Expected => "Initialization_Error");

      Tests.Test (Unit     => Get_Width'Access,
                  Name     => "Get_Width",
                  Expected => "Initialization_Error");

      Tests.Test (Unit     => New_Page'Access,
                  Name     => "New_Page",
                  Expected => "Initialization_Error");

      Tests.Test (Unit     => Set_Title'Access,
                  Name     => "Set_Title",
                  Expected => "Initialization_Error");

      Tests.Safe_Run (Initialize'Access);

      Tests.Test (Unit     => Get_Height'Access,
                  Name     => "Get_Height",
                  Expected => "@20",
                  Time_It  => False);

      Tests.Test (Unit     => Get_Width'Access,
                  Name     => "Get_Width",
                  Expected => "@50",
                  Time_It  => False);

      Tests.Test (Unit     => New_Page'Access,
                  Name     => "New_Page",
                  Expected => "<>");

      Tests.Test (Unit     => Set_Title'Access,
                  Name     => "Set_Title",
                  Expected => "<>");

   end Main;

end Test_Malef;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
