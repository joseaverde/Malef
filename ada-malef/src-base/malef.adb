-------------------------------------------------------------------------------
--                                                                           --
--                             M A L E F . A D B                             --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2021 José Antonio Verde Jiménez  All Rights Reserved  --
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
with Malef.Exceptions;
with Malef.Systems;
with Malef.Surfaces;


package body Malef is


   --====-------------------------------------====--
   --====-- INITIALIZATION AND FINALIZATION --====--
   --====-------------------------------------====--

   Initialize_Lock : Boolean := False;
   procedure Initialize is
   begin

      if Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library has already been initialized!";
      end if;

      while Initialize_Lock loop
         null;
      end loop;

      Initialize_Lock := True;

      -- We prepare the terminal depending on the operating system.
      Malef.Systems.Initialize;
      Malef.Systems.Prepare_Terminal;

      -- We then get the size of the terminal.
      Malef.Systems.Get_Terminal_Size(Rows => Height,
                                      Cols => Width);

      -- TODO: We update the main surface.

      -- Finally we tell the user the terminal has been initialized.
      Has_Been_Initialized := True;

      Initialize_Lock := False;

   end Initialize;


   procedure Finalize is
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library hasn't been initialized yet!";
      end if;

      -- We restore the terminal and finalize everything.
      Malef.Systems.Restore_Terminal;
      Malef.Systems.Finalize;

      -- TODO: Move the cursor down.

      -- Finally, in no error was found, we tell the user that the terminal has
      -- been finalized.
      Has_Been_Initialized := False;

   end Finalize;


   function Is_Initialized return Boolean is
   begin

      return Has_Been_Initialized;

   end Is_Initialized;




   --====-----------------------------------====--
   --====-- TERMINAL CONTROL AND HANDLING --====--
   --====-----------------------------------====--


   function Get_Height return Row_Type is
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library hasn't been initialized yet!";
      end if;

      return Height;

   end Get_Height;


   function Get_Width return Col_Type is
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library hasn't been initialized yet!";
      end if;

      return Width;

   end Get_Width;


   procedure New_Page is
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library hasn't been initialized yet!";
      end if;

      -- TODO: Use system's function.
      Ada.Text_IO.New_Line(Ada.Text_IO.Count(Height));

   end New_Page;


   procedure Set_Title (Name : String) is
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library hasn't been initialized yet!";
      end if;

      Malef.Systems.Set_Title (Name => Name);

   end Set_Title;


   function Update_Terminal_Size return Boolean is
      New_Height : Row_Type;
      New_Width  : Col_Type;
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library hasn't been initialized yet!";
      end if;

      Malef.Systems.Get_Terminal_Size(Rows => New_Height,
                                      Cols => New_Width);

      if New_Height /= Height or New_Width /= Width then
         Height := New_Height;
         Width  := New_Width;
         return True;
      end if;

      return False;

   end Update_Terminal_Size;



--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
--*--*- private -*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-



   function Get_Shared_Surface (Object : Malef.Surfaces.Surface_Type)
                                return Shared_Surface_Access is
   begin

      return Malef.Surfaces.Get_Reference (Object => Object);

   end Get_Shared_Surface;


end Malef;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
