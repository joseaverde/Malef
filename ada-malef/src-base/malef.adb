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

with Ada.Unchecked_Deallocation;
with Malef.Exceptions;
with Malef.Surfaces;
with Malef.Systems;


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

      Malef.Update_Main_Surface;

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
         Malef.Update_Main_Surface;
         return True;
      end if;

      return False;

   end Update_Terminal_Size;


   use Malef.Systems;
   procedure New_Page is
   begin
      Loaded_Subsystems(Current_Subsystem).New_Page;
   end New_Page;

   procedure Set_Title (Name : String) is
   begin
      Loaded_Subsystems(Current_Subsystem).Set_Title (Name);
   end Set_Title;


   procedure Clear_Screen is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Screen;
   end Clear_Screen;

   procedure Clear_Until_End_Of_Screen is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Until_End_Of_Screen;
   end Clear_Until_End_Of_Screen;

   procedure Clear_Until_Start_Of_Screen is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Until_Start_Of_Screen;
   end Clear_Until_Start_Of_Screen;

   procedure Clear_Entire_Screen is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Entire_Screen;
   end Clear_Entire_Screen;


   procedure Clear_Current_Line is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Current_Line;
   end Clear_Current_Line;

   procedure Clear_Until_End_Of_Line is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Until_End_Of_Line;
   end Clear_Until_End_Of_Line;

   procedure Clear_Until_Start_Of_Line is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Until_Start_Of_Line;
   end Clear_Until_Start_Of_Line;

   procedure Clear_Entire_Line is
   begin
      Loaded_Subsystems(Current_Subsystem).Clear_Entire_Line;
   end Clear_Entire_Line;


   procedure Enable_Line_Wrapping is
   begin
      Line_Wrapping := True;
      Loaded_Subsystems(Current_Subsystem).Enable_Line_Wrapping;
   end Enable_Line_Wrapping;

   procedure Disable_Line_Wrapping is
   begin
      Line_Wrapping := False;
      Loaded_Subsystems(Current_Subsystem).Disable_Line_Wrapping;
   end Disable_Line_Wrapping;

   function Has_Line_Wrapping return Boolean is
   begin
      return Line_Wrapping;
   end Has_Line_Wrapping;


   procedure Make_Cursor_Visible is
   begin
      Cursor_Visibility := True;
      Loaded_Subsystems(Current_Subsystem).Make_Cursor_Visible;
   end Make_Cursor_Visible;

   procedure Make_Cursor_Invisible is
   begin
      Cursor_Visibility := False;
      Loaded_Subsystems(Current_Subsystem).Make_Cursor_Invisible;
   end Make_Cursor_Invisible;

   function Is_Cursor_Visible return Boolean is
   begin
      return Cursor_Visibility;
   end Is_Cursor_Visible;


   procedure Save_Screen is
   begin
      Saved_Screen := True;
      Loaded_Subsystems(Current_Subsystem).Save_Screen;
   end Save_Screen;

   procedure Restore_Screen is
   begin
      if not Saved_Screen then
         raise Malef.Exceptions.Initialization_Error with
         "Cant restore non-saved screen!";
      end if;
      Loaded_Subsystems(Current_Subsystem).Restore_Screen;
   end Restore_Screen;

   function Has_Saved_Screen return Boolean is
   begin
      return Saved_Screen;
   end Has_Saved_Screen;



--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
--*--*- private -*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-
--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*-


   procedure Update_Main_Surface is
      Object : Malef.Surfaces.Surface_Type := Surfaces.Get_Main_Surface;
   begin

      Malef.Surfaces.Resize (Object, Height, Width);

   end Update_Main_Surface;


   overriding
   procedure Initialize (Object : in out Base_Type) is
   begin

      Reference (Object.Reference);

   end Initialize;


   overriding
   procedure Adjust (Object : in out Base_Type) is
   begin

      Reference (Object.Reference);

   end Adjust;


   overriding
   procedure Finalize (Object : in out Base_Type) is
      Old_Reference : constant not null Shared_Surface_Access
                    := Object.Reference;
   begin

      if Old_Reference /= Shared_Null_Surface'Access and
         Old_Reference /= Shared_Main_Surface'Access
      then
         -- This is used to avoid finalising the same object twice.
         Object.Reference := Shared_Null_Surface'Access;
         Unreference (Old_Reference);
         -- TODO: Remove it from all the object referencing it.
      end if;

   end Finalize;


   procedure Reference (Item : not null Shared_Surface_Access) is
   begin

      if Item = Shared_Null_Surface'Access or
         Item = Shared_Main_Surface'Access
      then
         return;
      end if;

      System.Atomic_Counters.Increment (Item.Counter);

   end Reference;



   procedure Unreference (Item : not null Shared_Surface_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Shared_Surface_Type,
                                                        Shared_Surface_Access);
      procedure Free is new Ada.Unchecked_Deallocation (Matrix_Type,
                                                        Matrix_Access);
      Old : Shared_Surface_Access := Item;
   begin

      if Old = Shared_Null_Surface'Access or
         Old = Shared_Main_Surface'Access
      then
         return;
      end if;

      if System.Atomic_Counters.Decrement (Old.Counter) then
         Free (Old.Grid);
         Free (Old);
      end if;

   end Unreference;


   function Get_Reference (Object : Base_Type)
                           return Shared_Surface_Access is
   begin

      return Object.Reference;

   end Get_Reference;


end Malef;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
