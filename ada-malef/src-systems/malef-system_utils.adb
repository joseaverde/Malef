-------------------------------------------------------------------------------
--                                                                           --
--                M A L E F - S Y S T E M _ U T I L S . A D B                --
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

with Ada.Directories;
with Ada.Environment_Variables;
with Ada.IO_Exceptions;

package body Malef.System_utils is

   function Get_Path (Programme_Name                 : String;
                      PATH_Environment_Variable_Name : String := "PATH";
                      Default_PATHS                  : String := "/bin";
                      Separator                      : Character := ':')
                      return String is
      PATH : constant String := (if Ada.Environment_Variables.Exists(
                                    PATH_Environment_Variable_Name) then
                                    Ada.Environment_Variables.Value("PATH")
                                 else
                                    Default_PATHS);
      First : Positive := PATH'First;
      Last  : Positive := First;

      Searcher  : Ada.Directories.Search_Type;
      Dir_Entry : Ada.Directories.Directory_Entry_Type;
   begin

      while Last <= PATH'Last loop
         if PATH(Last) = Separator or Last = PATH'Last then
            Last := Last - 1;
            Find_In_Path:
               declare
               begin
                  Ada.Directories.Start_Search(Search    => Searcher,
                                               Directory => PATH(First..Last),
                                               Pattern   => Programme_Name);
                  while Ada.Directories.More_Entries(Search => Searcher) loop
                     Ada.Directories.Get_Next_Entry(
                        Search          => Searcher,
                        Directory_Entry => Dir_Entry);
                     if Ada.Directories.Simple_Name(Dir_Entry) = Programme_Name
                     then
                        return PATH(First .. Last);
                     end if;
                  end loop;
               exception
                  when Ada.IO_Exceptions.Name_Error =>
                     null;
               end Find_In_Path;
            Last  := Last + 1;
            First := Last + 1;
         end if;
         Last := Last + 1;
      end loop;

      -- Couldn't find the commands, we return this so the Windows part doesn't
      -- become stupid. No error is raised until real initialization.
      return "";

   end Get_Path;


   function To_String (C : Color_Component_Type)
                       return String is
   begin

      return Color_Components_Strings(C);

   end To_String;


end Malef.System_utils;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
