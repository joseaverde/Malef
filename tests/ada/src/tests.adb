-------------------------------------------------------------------------------
--                                                                           --
--                             T E S T S . A D B                             --
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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;

package body Tests is

   procedure Put_Line (Item : String) with Inline is
   begin

      Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
                           Item => Item);

   end Put_Line;


   procedure Safe_Run (Unit : Unit_Type) is
   begin

      Try:
         declare
            Dummy : constant String := Unit.all;
         begin
            null;
         end Try;

   exception
      when others =>
         null;
   end Safe_Run;


   procedure Start (Pkg : String := "<>") is
   begin

      if Pkg = "<>" then
         Put_Line ("language" & TAB & "ada");
      else
         Put_Line ("package" & TAB & Pkg);
      end if;

   end Start;


   procedure Test (Unit     : Unit_Type;
                   Name     : String;
                   Expected : String  := "<>";
                   Time_It  : Boolean := True) is

      use type Ada.Calendar.Time;
      Start  : Ada.Calendar.Time;
      Finish : Ada.Calendar.Time;
      procedure Put_Data (Got : String) with Inline is
      begin

         Put_Line((if Got = Expected
         then
            "passed" & TAB & Name & TAB & (if Time_It
                                           then Duration'Image(Finish - Start)
                                           else "<null>")
         else
            "failed" & TAB & Name & TAB & Expected & TAB & Got));

      end Put_Data;

   begin

      if Avoid_Delays and not Time_It then
         Put_Line ("skipped" & TAB & "Name");
         return;
      end if;
      Start := Ada.Calendar.Clock;
      Try:
         declare
            Output : constant String := Unit.all;
         begin
            Finish := Ada.Calendar.Clock;
            Put_Data (Output);
         end Try;

   exception
      when Error : others =>
         Put_Data (Ada.Exceptions.Exception_Name(Error));
   end Test;


   procedure Wait (Text : String) is
      Char : Character;
   begin

      Ada.Text_IO.Put(Text);
      Ada.Text_IO.Get_Immediate(Char);
      Ada.Text_IO.New_Line;

   end Wait;


   procedure Wrap (Proc : Proc_Type;
                   Name : String) is
   begin

      Start (Name);
      Proc.all;

   exception
      when others =>
         Put_Line("fatal" & TAB & Name);
   end Wrap;

end Tests;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
