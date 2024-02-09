-------------------------------------------------------------------------------
--                                                                           --
--                    M A L E F - D E B U G _ I O . A D S                    --
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

with Malef.Widgets;
with Malef.Surfaces;

package Malef.Debug_IO with Preelaborate is

   -- This package's purpose is providing a simple and powerful interface for
   -- debuggin terminal based implementations. Using the usual *Text_IO
   -- packages is not possible, since writing to the screen destroys the
   -- interfaces. Instead you can decide to write to a file or to a Widget.
   --
   -- Imagine you want to debug your App. You would first instanciate an
   -- instance of the Debug nested package, like:
   --
   --    with Malef.Debug_IO;
   --
   --    package Debug_IO is new Malef.Debug_IO.Debug_IO (Name => "app.log");
   --
   -- If you want to remove the debugging on release mode you can:
   --
   --    with Malef.Debug_IO;
   --
   --    package Debug_IO is
   --       new Malef.Debug_IO.Debug_IO (
   --       Name => "app.log",
   --       Mode => Malef.Debug_IO.Release);
   --
   -- Choose Release or Debug mode depending on how you are building your
   -- application. If you are using alire and your app is called, for instance,
   -- `My_App`. You can:
   --
   --    with Malef.Debug_IO;
   --    with My_App_Config; use My_App_Config;
   --
   --    package Debug_IO is
   --       new Malef.Debug_IO.Debug_IO (
   --       Name => "app.log",
   --       Mode => (if Build_Profile = release
   --                   then Malef.Debug_IO.Release
   --                   else Malef.Debug_IO.Debug));
   --
   -- Everything you write, by default will be dumped to the same file.
   -- You can also write to a widget and display it within your application.
   --
   -- This unit is preelaborable, so you won't have many problems with it.
   -- Check the generic package parameters to see more things you can do.

   type Build_Mode is (Debug, Release);

   type Message_Severity is (Debug, Information, Warning, Error, Critical);
   -- There are different kinds of message severities. You can change the
   -- default severity any time. Then you can filter the message that match
   -- the severity levels you want.

   type Visible_Messages is array (Message_Severity) of Boolean;
   -- Filter array for message severity levels.

   generic
      Mode     : in Build_Mode       := Debug;
      Name     : in String           := "";
      Widget   : in Boolean          := False;
      Severity : in Message_Severity := Debug;
      Visible  : in Visible_Messages := (others => True) with Unreferenced;
   package Debug_IO is

      -- @param Mode
      -- The compilation mode. If Debug, everything works as expected.
      -- If release, everything is optimised out, there is no performance
      -- penalty and nothing is shown.
      --
      -- @param Name
      -- The name of the file to write the information to. It must have a value
      -- or the Widget parameter must be True (not both at the same time).
      --
      -- @param Widget
      -- If True, the messages are written to a custom Widget you can open
      -- inside your application to debug. If this value is True, Name should
      -- be an empty String. If True, you will be able to use the New_Debug
      -- function to get a valid Widget.
      --
      -- @param Severity
      -- The default severity level.
      --
      -- @param Visible
      -- The default visibility rules of messages.

      pragma Assert (Widget xor Name /= "");

      procedure Put (
         Kind : in Message_Severity;
         Item : in String);

      procedure Wide_Put (
         Kind : in Message_Severity;
         Item : in Wide_String);

      procedure Wide_Wide_Put (
         Kind : in Message_Severity;
         Item : in Wide_Wide_String);

      procedure Put (
         Item : in String);

      procedure Wide_Put (
         Item : in Wide_String);

      procedure Wide_Wide_Put (
         Item : in Wide_Wide_String);

      type Debug_Widget is
         new Widgets.Widget with
         private with
         Default_Initial_Condition => False;

      overriding
      procedure On_Draw (
         Object  : in     Debug_Widget;
         Surface : in out Surfaces.Surface;
         Area    : in     Widgets.Draw_Area);

      function New_Debug
         return Debug_Widget;

   private

      type Debug_Widget is
         new Widgets.Widget with
         null record;

   end Debug_IO;

end Malef.Debug_IO;
