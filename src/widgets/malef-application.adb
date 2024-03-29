-------------------------------------------------------------------------------
--                                                                           --
--                 M A L E F - A P P L I C A T I O N . A D B                 --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   B O D Y                              --
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

with Malef.System;
with Malef.Palettes;
with Malef.Surfaces;
with Malef.Groups;

package body Malef.Application is

   package body Implementation is

      task body Application_Task is
      begin
         loop
            select
               accept Start;
               Malef.System.Initialize;
            or
               terminate;
            end select;
            accept Stop;
            Malef.System.Finalize;
         end loop;
      end Application_Task;

   end Implementation;

   protected body Application is

      procedure Update_Background is
         procedure Set_Background (
            Object : aliased in out Groups.Group)
         is
            Surface : Surfaces.Surface (Height, Width);
         begin
            Surface.Fill (' ');
            Surface.Fill_Background (Palettes.Green);
            Object.Insert (1, Surface);
         end Set_Background;
      begin
         Window.Window.Process_Group (Set_Background'Access);
         Window.Window.Display;
      end Update_Background;

      procedure Initialize is
         use type Groups.Layer_Count;
      begin
         if Initialized then
            return;
         end if;
         -- NOTE: It can never be blocking, it can only be initialised once
         --       (it is in the protected body).
         pragma Warnings (Off,
            "potentially blocking operation in protected operation");
         App_Task.Start;
         pragma Warnings (On,
            "potentially blocking operation in protected operation");
         Window.Callbacks.Register (
            Event    => Events.Resize_Event,
            Observer => Observer'Access,
            Callback => When_Resized'Access);
         Window.Window.Get_Dimensions (Height, Width);
         Available := [others => False];
         Window.Window.Set_Group (Groups.Empty (Max_Dialogs + 1));
         Update_Background;
         Initialized := True;
      end Initialize;

      procedure Finalize is
      begin
         if not Initialized then
            return;
         end if;
         pragma Warnings (Off,
            "potentially blocking operation in protected operation");
         App_Task.Stop;
         pragma Warnings (On,
            "potentially blocking operation in protected operation");
         Initialized := False;
      end Finalize;

      procedure Set_Style_Sheet (
         Sheet : in Malef.Styles.Sheets.Style_Sheet) is
      begin
         Style := Sheet;
         -- Update_Dialogs;
      end Set_Style_Sheet;

      procedure Add (
         Object : in Malef.Dialogs.Dialog;
         Modal  : in Boolean := False with Unreferenced)
      is
         procedure Draw (
            Group : aliased in out Groups.Group)
         is
            use type Malef.Dialogs.Window_Mode;
            D_Rows : Positive_Row_Count;
            D_Cols : Positive_Col_Count;
         begin
            if Object.Get_Mode = Malef.Dialogs.Windowed then
               Object.Get_Size (D_Rows, D_Cols);
            else
               D_Rows := 24;
               D_Cols := 80;
            end if;
            declare
               Surface : Surfaces.Surface (D_Rows, D_Cols);
            begin
               Object.Draw (Surface);
               Group.Insert (2, Surface);
            end;
         end Draw;
      begin
         Window.Window.Process_Group (Draw'Access);
         Window.Window.Display;
      end Add;

      procedure When_Resized (
         Observer : aliased in out Window.Event_Observer'Class;
         Event    :         in     Events.Event_Type)
      is
         pragma Unreferenced (Observer);
      begin
         Height := Event.New_Size.Row;
         Width := Event.New_Size.Col;
         Update_Background;
      end When_Resized;

   end Application;

begin

   Malef.System.Initialize;
   Application.Initialize;

end Malef.Application;
