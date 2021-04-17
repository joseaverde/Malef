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
with Malef.Systems.Utils; use Malef.Systems.Utils;


package body Malef.Subsystems.Ansi is

   type Subsystem_Controller is new Ada.Finalization.Limited_Controlled
      with null record;
   overriding procedure Initialize (SC : in out Subsystem_Controller);
   overriding procedure Finalize   (SC : in out Subsystem_Controller);

   Subsystem_Handler : aliased Subsystem;

   Lock : Boolean := True;
   overriding
   procedure Put (Subsys : not null access Subsystem;
                  Object : Shared_Surface_Access) is
      Last_Format : Format_Type := Default_Format;
   begin

      -- We wait until the operation is unlocked because we can't put two
      -- surfaces onto the screen at the same time, because they may interfere.
      -- This is done so it's safe for multitasking.
      while Lock loop null; end loop;
      Lock := True;

      Std_Out.Write (Get_Format(Last_Format));
      for Row in Object.Grid'Range(1) loop
         for Col in Object.Grid'Range(2) loop
            if Last_Format /= Object.Grid(Row, Col).Format then
               Std_Out.Write (Get_Clear);
               Last_Format := Object.Grid(Row, Col).Format;
            end if;

            Std_Out.Write (Get_Format(Last_Format));
            case Object.Grid(Row, Col).Char is
               when 0 .. 31 | 127 =>
                  -- Non printable characters.
                  -- TODO: Special treatment.
                  null;
               when others =>
                  -- We just write them.
                  Std_Out.Write (Object.Grid(Row, Col).Char);
            end case;
         end loop;
      end loop;

      -- We dump the buffer and unlock it.
      Std_Out.Dump;
      Lock := False;

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



   function Get_Color_1  (Foreground : Color_Type;
                          Background : Color_Type)
                          return String is
   begin

      return "";

   end Get_Color_1;


   function Get_Color_3  (Foreground : Color_Type;
                          Background : Color_Type)
                          return String is
   begin

      return "TODO";

   end Get_Color_3;


   function Get_Color_4  (Foreground : Color_Type;
                          Background : Color_Type)
                          return String is
   begin

      return "TODO";

   end Get_Color_4;


   function Get_Color_8  (Foreground : Color_Type;
                          Background : Color_Type)
                          return String is
   begin

      return "TODO";

   end Get_Color_8;


   function Get_Color_24 (Foreground : Color_Type;
                          Background : Color_Type)
                          return String is
   begin

      return ASCII.ESC & '[' &
               "38;2;" & To_String(Foreground(R)) & ';' &
                         To_String(Foreground(G)) & ';' &
                         To_String(Foreground(B)) & ';' &
               "48;2;" & To_String(Background(R)) & ';' &
                         To_String(Background(G)) & ';' &
                         To_String(Background(B)) &
               'm';

   end Get_Color_24;


   function Get_Style (Style : Style_Array)
                       return String is
   begin


      return "TODO";

   end Get_Style;


   function Get_Move (Coord : Coord_Type)
                      return String is
   begin

      return "TODO";

   end Get_Move;


   function Get_Format (Format : Format_Type)
                        return String is
   begin

      -- TODO: Change this.
      return Get_Color_24 (
               Foreground => Format.Foreground_Color,
               Background => Format.Background_Color
             ) &
             Get_Style (Format.Styles);

   end Get_Format;


   function Get_Clear return String is
   begin

      return ASCII.ESC & "[0m";

   end Get_Clear;



   overriding
   procedure Initialize (SC : in out Subsystem_Controller) is
   begin

      Lock := False;
      Malef.Systems.Loaded_Subsystems(Malef.ANSI) :=
         Subsystem_Handler'Access;

   end Initialize;


   overriding
   procedure Finalize (SC : in out Subsystem_Controller) is
   begin

      Lock := True;
      Malef.Systems.Loaded_Subsystems(Malef.ANSI) := null;

   end Finalize;

   pragma Warnings (Off);
   SC : Subsystem_Controller;
   pragma Warnings (On);

end Malef.Subsystems.Ansi;

 
---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
