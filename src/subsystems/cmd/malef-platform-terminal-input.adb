-------------------------------------------------------------------------------
--                                                                           --
--     M A L E F - P L A T F O R M - T E R M I N A L - I N P U T . A D B     --
--                                                                           --
--                                 M A L E F                                 --
--                                   C M D                                   --
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

with Ada.Unchecked_Deallocation;
with Malef.Platform.Win32;
with Malef.Debug;

package body Malef.Platform.Terminal.Input is

   task type Input_Task is
   end Input_Task;

   type Input_Task_Access is access Input_Task;

   Input           : Input_Task_Access := null;
   Standard_Output : Win32.HANDLE;

   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Input_Task,
      Name   => Input_Task_Access);

   procedure Initialize is
   begin
      Standard_Output := Win32.GetStdHandle (Win32.STD_OUTPUT_HANDLE);
      Input := new Input_Task;
   end Initialize;

   procedure Finalize is
   begin
      abort Input.all;
      Free (Input);
   end Finalize;

   -- task body Input_Task is
   --    use Malef.Events;
   --    use Malef.Platform.Events;
   --    Key : Key_Type;
   -- begin
   --    loop
   --       Catch_End_Error : declare
   --       begin
   --          Get_Immediate (Wide_Wide_Character (Key));
   --          if Key = Key_Type'Val (0) then
   --             Key := Key_Unknown;
   --          elsif Key = Key_Type'Val (27) then
   --             -- It is a sequence.
   --             Key := Key_Unknown;
   --          end if;
   --          Queue.Enqueue (+Event_Type'(Name => Keyboard_Event,
   --                                      Time => From_Start,
   --                                      Key  => Key));
   --       exception
   --          when End_Error =>
   --             Queue.Enqueue (+Event_Type'(Time => From_Start,
   --                                         Name => Input_Closed));
   --       end Catch_End_Error;
   --    end loop;
   -- end Input_Task;

   task body Input_Task is
   begin
      null;
   end Input_Task;

   use Win32;

   function GetConsoleScreenBufferInfo (
      hConsoleOutput            : in     HANDLE;
      lpConsoleScreenBufferInfo :    out CONSOLE_SCREEN_BUFFER_INFO)
      return BOOL with
      Import        => True,
      Convention    => Stdcall,
      External_Name => "GetConsoleScreenBufferInfo";

   procedure Get_Dimensions (
      Rows : out Positive_Row_Count;
      Cols : out Positive_Col_Count)
   is
      use type BOOL;
      use type SHORT;
      csbi : CONSOLE_SCREEN_BUFFER_INFO;
   begin
      if W_FALSE = GetConsoleScreenBufferInfo (Standard_Output, csbi) then
         Debug.Put (Debug.Types.Critical,
                    "GetConsoleScreenBufferInfo failed with error:"
                    & GetLastError'Image);
         Rows := Default_Rows;
         Cols := Default_Cols;
      else
         Rows := Row_Type (csbi.srWindow.Bottom - csbi.srWindow.Top + 1);
         Cols := Col_Type (csbi.srWindow.Right - csbi.srWindow.Left + 1);
      end if;
   end Get_Dimensions;

end Malef.Platform.Terminal.Input;
