-------------------------------------------------------------------------------
--                                                                           --
--                 M A L E F - A P P L I C A T I O N . A D S                 --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   S P E C                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2021-2024 José Antonio Verde Jiménez  All Rights Reserved  --
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

with Malef.Window;
with Malef.Dialogs;
with Malef.Events;
with Malef.Styles.Sheets;
with Malef.Styles.Compat;

package Malef.Application is

   pragma Elaborate_Body;

   Max_Dialogs : constant := 16;

   package Implementation is

      type Window_Observer is
         new Window.Event_Observer with
         null record;

      type Boolean_Array is array (1 .. Max_Dialogs) of Boolean;
      type Dialog_Array is array (1 .. Max_Dialogs) of Dialogs.Dialog;

      task type Application_Task is
         entry Start;
         entry Stop;
      end Application_Task;

   end Implementation;

   protected Application is

      procedure Initialize;

      procedure Finalize;

      procedure Set_Style_Sheet (
         Sheet : in Malef.Styles.Sheets.Style_Sheet);

      procedure Add (
         Object : in Dialogs.Dialog;
         Modal  : in Boolean := False);

   private

      procedure When_Resized (
         Observer : aliased in out Window.Event_Observer'Class;
         Event    :         in     Events.Event_Type);

      Available    : Implementation.Boolean_Array;
      Dialogs      : Implementation.Dialog_Array;
      Initialized  : Boolean := False;
      Observer     : aliased Implementation.Window_Observer;
      Height       : Positive_Row_Count := 24;
      Width        : Positive_Col_Count := 80;
      Style        : Malef.Styles.Sheets.Style_Sheet
                   := Malef.Styles.Compat.Style;
      App_Task     : Implementation.Application_Task;

   end Application;

end Malef.Application;
