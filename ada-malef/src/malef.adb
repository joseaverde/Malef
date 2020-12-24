-------------------------------------------------------------------------------
--                                                                           --
--                             M A L E F . A D B                             --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
--                                                                           --
-------------------------------------------------------------------------------
--     Copyright (c) 2020 José Antonio Verde Jiménez All Rights Reserved     --
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

with Ada.Interrupts;
with Ada.Text_IO;
with Malef.Events;
with Malef.Exceptions;
with Malef.Surfaces;

with Malef.Linux;
with Malef.Windows;

package body Malef is

   --====-------------------------------------====--
   --====-- INITIALIZATION AND FINALIZATION --====--
   --====-------------------------------------====--

   -- TODO: Lock it for multitasking
   procedure Initialize (Info : Initialization_Information_Type) is
   begin

      if Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library has already been initialized!";
      end if;

      -- We set up the information.
      Current_Info := Info;

      -- We retrieve the required functions and procedures for it to work in
      -- the given system with the given information.
      case Info.Operating_System is
         when GNU_Linux_OS =>
            Prepare_Terminal   := Malef.Linux.Prepare_Terminal 'Access;
            Restore_Terminal   := Malef.Linux.Restore_Terminal 'Access;
            Get_Terminal_Size  := Malef.Linux.Get_Terminal_Size'Access;
            Terminal_Set_Title := Malef.Linux.Set_Title        'Access;
         when Windows_OS =>
            Prepare_Terminal   := Malef.Windows.Prepare_Terminal 'Access;
            Restore_Terminal   := Malef.Windows.Restore_Terminal 'Access;
         -- Get_Terminal_Size  := Malef.Windows.Get_Terminal_Size'Access;
         -- Terminal_Set_Title := Malef.Windows.Set_Title        'Access;
         when others =>
            -- TODO
            Prepare_Terminal   := Malef.Linux.Prepare_Terminal 'Access;
            Restore_Terminal   := Malef.Linux.Restore_Terminal 'Access;
            Get_Terminal_Size  := Malef.Linux.Get_Terminal_Size'Access;
            Terminal_Set_Title := Malef.Linux.Set_Title        'Access;
      end case;
      
      -- We prepare the terminal depending on the operating system.
      Prepare_Terminal.all;

      -- We then get the size of the terminal.
      Get_Terminal_Size.all(R => Height,
                            C => Width);

      -- Then it's time for events.
      case Info.Operating_System is
         when GNU_Linux_OS =>
            Ada.Interrupts.Attach_Handler(
               New_Handler => Malef.Events.Event_Handler.
                                 Update_Terminal_Size'Access,
               Interrupt   => Malef.Linux.SIGWINCH);
         when Windows_OS =>
            null;
         when others =>
            null; -- TODO
      end case;

      -- Finally we tell the user the terminal has been initialized.
      Has_Been_Initialized := True;

      -- We then prepare a new page for the terminal.
      New_Page;

   end Initialize;


   procedure Finalize is
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library hasn't been initialized yet!";
      end if;

      -- We restore the terminal.
      Restore_Terminal.all;

      -- We then detach the event handlers.
      case Current_Info.Operating_System is
         when GNU_Linux_OS =>
            Ada.Interrupts.Detach_Handler(Interrupt => Malef.Linux.SIGWINCH);
         when Windows_OS =>
            null;
         when others =>
            null; -- TODO
      end case;

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

      -- TODO: Use IO package.
      Ada.Text_IO.New_Line(Ada.Text_IO.Count(Height));

   end New_Page;


   procedure Set_Title (Name : String) is
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library hasn't been initialized yet!";
      end if;

      Terminal_Set_Title.all (S => Name);

   end Set_Title;


   function Update_Terminal_Size return Boolean is
      New_Height : Row_Type;
      New_Width  : Col_Type;
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The Malef library hasn't been initialized yet!";
      end if;

      Get_Terminal_Size.all(R => New_Height,
                            C => New_Width);

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
