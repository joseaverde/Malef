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
   -- `My_App'. You can:
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

   type Visible_Messages is array (Message_Severity) of Boolean;

   generic
      Mode     : in Build_Mode       := Debug;
      Name     : in String           := "";
      Widget   : in Boolean          := False;
      Severity : in Message_Severity := Debug;
      Visible  : in Visible_Messages := (others => True);
   package Debug_IO is

      pragma Assert (Widget xor Name /= "");

      Default_Severity   : Message_Severity := Severity;
      Default_Visibility : Visible_Messages := Visible;

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
         private;

      procedure On_Draw (
         Object  : in     Debug_Widget;
         Surface : in out Surfaces.Surface;
         Area    : in     Widgets.Draw_Area);

      function New_Debug
         return Debug_Widget with
         Pre => Widget = True;

   private

      type Debug_Widget is
         new Widgets.Widget with
         null record;

   end Debug_IO;

end Malef.Debug_IO;
