-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - S Y S T E M S . A D S                     --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
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

with Malef.Subsystems;
with System;

--
-- @summary
-- These are system specific information.
--
-- @description
-- When talking about systems in this library I'm not talking about operaing
-- systems, but systems itself. For example GNU/Linux, Windows are operating
-- systems but are also considered systems. However there are also the so
-- called subsystems, i.e. where the program will be runned inside the
-- systems. For example, using ANSI escape sequences, the windows CMD and
-- maybe using the ncurses library in a future. Moreover maybe in a future
-- even the web browser will be considered a system. This library depends
-- on the subsystems library.
-- Therefore the specification must be the same for every system out there
-- (even if the implementation is different).
-- This package is private so the user can't use it before initialization
-- (some of the functions may work in systems like Linux before initialization
-- but are fatal in systems like Windows).
--
private package Malef.Systems is

   type Library_Handle is new System.Address;

   --====-----------------------------------====--
   --====-- INITIALIZATION / FINALIZATION --====--
   --====-----------------------------------====--
   --
   -- This operations must be runned before starting to work with a terminal
   -- or a console to prepare a protected and controlled environment. After
   -- that, the user should finalize everything because the terminal might
   -- break.
   --

   --
   -- This procedure initializes everything needed to be initialized in the
   -- library, i.e, before calling Prepare_Terminal. Keep in mind some systems
   -- might give a null implementation.
   --
   -- @exception Malef.Exception.Initialization_Error
   -- This exception is raised if the library couldn't be initialized, before
   -- raising the exception it will try to recover the terminal as much as
   -- possible.
   --
   procedure Initialize;

   --
   -- This procedure finalizes everything needed to be finalized in the library
   -- i.e. after calling Restore_Terminal. Keep in ming some (and most systems)
   -- might give a null implementation.
   --
   procedure Finalize;

   --
   -- This procedure prepares the terminal/console.
   --
   -- @exception Malef.Exception.Initialization_Error
   -- This exceptions is raised if the terminal couldn't be prepared. There
   -- will be an attempt to restore any possible damages to the terminal or
   -- the console themselves.
   --
   procedure Prepare_Terminal;

   --
   -- This procedure cleans up and restores the previous terminal/console and
   -- its previous configuration.
   --
   -- @exception Malef.Exceptions.Initialization_Error
   -- This exception is raised if the terminal couldn't be restored. Before
   -- raising it, the terminal will be restored as much as possible though.
   --
   procedure Restore_Terminal;

   -- TODO: This function is DESPRECIATED it will be removed in later versions.
   function Get_Format (Format : Format_Type)
                        return String;

   procedure Get_Terminal_Size (Rows : out Row_Type;
                                Cols : out Col_Type);


   --====---------------====--
   --====-- INTERNALS --====--
   --====---------------====--

   Saved_Screen      : Boolean := False;
   Line_Wrapping     : Boolean := False;
   Cursor_Visibility : Boolean := False;

   Loaded_Subsystems_Handles : array (Subsystem_Kind'Range)
                               of Library_Handle
                             :=(others => Library_Handle(System.Null_Address));
   Loaded_Subsystems : array (Subsystem_Kind'Range)
                       of Malef.Subsystems.Subsystem_Access
                     := (
      None   => Malef.Subsystems.None.Subsystem_Handler'Access,
      Choose => Malef.Subsystems.None.Subsystem_Handler'Access,
      others => null);
   Current_Subsystem : Subsystem_Kind := Choose;


   Available_Styles : array (Style_Type'Range) of Boolean := (others => False);

   type Color_Bits is (Bit1, Bit3, Bit4, Bit8, Bit24);
   Available_Colors : array (Color_Bits'Range) of Boolean := (Bit1   => True,
                                                              others => False);

end Malef.Systems;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
