-------------------------------------------------------------------------------
--                                                                           --
--             M A L E F - S U B S Y S T E M S - A N S I . A D B             --
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

with Ada.Finalization;
with Malef.Systems;
with Malef.Systems.Utils;


package body Malef.Subsystems.Ansi is

   type Subsystem_Controller is new Ada.Finalization.Limited_Controlled
      with null record;
   overriding procedure Initialize (SC : in out Subsystem_Controller);
   overriding procedure Finalize   (SC : in out Subsystem_Controller);

   Subsystem_Handler : aliased Subsystem;


   overriding
   procedure Put (Subsys : not null access Subsystem;
                  Object : Shared_Surface_Access) is
   begin

      -- TODO
      null;

   end Put;


   overriding
   function Get_Format (Subsys : not null access Subsystem;
                        Format : Format_Type)
                        return String is
      function To_String (C : Color_Component_Type) return String
         renames Malef.Systems.Utils.To_String;
   begin

      -- TODO: This function is still unfinished, it only returns colours.
      --       Also optimize it.

      return ASCII.ESC & '[' &
               "38;2;" & To_String(Format.Foreground_Color(R)) & ';' &
                         To_String(Format.Foreground_Color(G)) & ';' &
                         To_String(Format.Foreground_Color(B)) & ';' &
               "48;2;" & To_String(Format.Background_Color(R)) & ';' &
                         To_String(Format.Background_Color(G)) & ';' &
                         To_String(Format.Background_Color(B)) &
               'm';

   end Get_Format;


   overriding
   procedure Initialize (SC : in out Subsystem_Controller) is
   begin

      Malef.Systems.Loaded_Subsystems(Malef.ANSI) :=
         Subsystem_Handler'Access;

   end Initialize;


   overriding
   procedure Finalize (SC : in out Subsystem_Controller) is
   begin

      Malef.Systems.Loaded_Subsystems(Malef.ANSI) := null;

   end Finalize;

   pragma Warnings (Off);
   SC : Subsystem_Controller;
   pragma Warnings (On);

end Malef.Subsystems.Ansi;

 
---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
