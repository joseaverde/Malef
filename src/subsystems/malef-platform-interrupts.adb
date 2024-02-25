-------------------------------------------------------------------------------
--                                                                           --
--         M A L E F - P L A T F O R M - I N T E R R U P T S . A D B         --
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

with Malef.Events;
with Malef.Platform.Events;
with Malef.Platform.Terminal;
with Malef.Platform.Terminal.Input;

package body Malef.Platform.Interrupts is

   use Malef.Events;
   use Malef.Platform.Events;

   protected Event_Handler is

      procedure On_Resize;
      pragma Interrupt_Handler (On_Resize);

      procedure On_Cancel;
      pragma Interrupt_Handler (On_Cancel);

      procedure On_Kill;
      pragma Interrupt_Handler (On_Kill);

   end Event_Handler;

   protected body Event_Handler is

      pragma Warnings (Off,
         "potentially blocking operation in protected operation");
      procedure On_Resize is
         Size : Cursor_Type;
      begin
         Terminal.Input.Get_Dimensions (Size.Row, Size.Col);
         Queue.Enqueue (+Event_Type'(New_Size => Size,
                                     Time     => Terminal.From_Start,
                                     Name     => Resize_Event));
      end On_Resize;

      procedure On_Cancel is
      begin
         Queue.Enqueue (+Event_Type'(Time => Terminal.From_Start,
                                     Name => Cancel_Event));
      end On_Cancel;

      procedure On_Kill is
      begin
         Queue.Enqueue (+Event_Type'(Time => Terminal.From_Start,
                                     Name => Kill_Event));
      end On_Kill;
      pragma Warnings (On,
         "potentially blocking operation in protected operation");

   end Event_Handler;

   Old_Handlers : array (Interrupt_Name)
                  of Ada.Interrupts.Parameterless_Handler;
   New_Handlers : constant array (Interrupt_Name)
                  of Ada.Interrupts.Parameterless_Handler
                  := [ Resize_Id => Event_Handler.On_Resize'Access
                     , Cancel_Id => Event_Handler.On_Cancel'Access
                     , Kill_Id   => Event_Handler.On_Kill'Access
                     ];

   procedure Attach is
      use type Ada.Interrupts.Interrupt_ID;
      Ids : constant Interrupt_Array := Get_Interrupts;
   begin
      for Kind in Interrupt_Name when Ids (Kind) /= No_Id loop
         Ada.Interrupts.Exchange_Handler (
            Old_Handler => Old_Handlers (Kind),
            New_Handler => New_Handlers (Kind),
            Interrupt   => Ids (Kind));
      end loop;
   end Attach;

   procedure Detach is
      use type Ada.Interrupts.Interrupt_ID;
      Ids : constant Interrupt_Array := Get_Interrupts;
   begin
      for Kind in Interrupt_Name when Ids (Kind) /= No_Id loop
         Ada.Interrupts.Attach_Handler (Old_Handlers (Kind), Ids (Kind));
      end loop;
   end Detach;

   function Get_Interrupts
      return Interrupt_Array is
      separate;

end Malef.Platform.Interrupts;
