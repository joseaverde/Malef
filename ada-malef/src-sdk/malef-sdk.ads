-------------------------------------------------------------------------------
--                                                                           --
--                         M A L E F - S D K . A D S                         --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  S P E C                                  --
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

with Malef.Boxes;
with Malef.Surfaces;

--
-- @summary
-- This package is meant to facilitate the creation of interactive porgrams
-- with the Malef API.
--
-- @description
-- This package declares a new type called Widget which is derived from the Box
-- type which helps with the creation of interactive applications such as
-- message boxes or other kind of things. This was planned to be added in
-- future versions but I needed it for a current project.
--
-- The internal mechanisms work in the following way:
--
-- Every Widget is an object derived from Widget_Type. Every Widget
-- has three functions Run, Update and Assign and a Return_Type. The
-- Return_Type is usually an enumeration type -- but it can be an integer
-- type -- which is returned by the Run function. Every possible return type
-- may have Assign'ed a Widget to it. When the user selects them these Widgets
-- will be runned (recursively). And they also contain an Update function
-- which is runned for all the Widgets, for example: if you have created a
-- menu: Menu -> File -> Open -> Select File. Then those four widgets will be
-- updated (a parameter Focused is passed so you can know if the user
-- currently interacting with that same widget.
--
-- Malef defines several Widgets so you can start working on your project
-- right away:
--
--  * Multy Widgets: This Widget is a set of Widgets.
--  * Function Widget: This Widget are just user-defined functions which
--  return Return_Type values.
--  * String Widget: This simple Widget is for getting user input.
--  * Message Widget: This Widget is like menus but contain a big message.
--  * Menu Widgets: Menu Widgets.
--
-- You can learn more about their usage in the different sources.
--
package Malef.SDK is

   type Horizontal_Alignment_Type is (
      Align_Left, Align_Right, Centered, Justified);
   type Vertical_Alignment_Type is (
      Align_Top, Align_Bottom, Centered);

   Default_Shadow_Color : constant Color_Type := (0, 0, 0, 128);

   generic
      -- The return type is either an Enumeration type or an Integer type which
      -- is returned by the Widget when it has interacted with the user you can
      -- use as return types things like:
      --    type Return_Type is (Ok);
      --    type Return_Type is (No, Yes);
      --    type Return_Type is Integer range 1 .. 3;
      type Return_Type is (<>);
   package Widgets is

      --type Return_Array is array (Return_Type'Range) of Widget_Type'Class;
      type Widget_Type is abstract new Base_Type with
         record
            Box     : Malef.Boxes.Box_Type;
            Surface : Malef.Surfaces.Surface_Type;
            Shadow  : Malef.Surfaces.Surface_Type;
            --Next    :
         end record;

      type Any_Widget_Access is not null access Widget_Type'Class;

      overriding
      function Get_Reference (Widget : in Widget_Type)
         return Surface_Reference;

      --
      -- This function when runned tells the user
      function Run (Widget : in out Widget_Type)
         return Return_Type is abstract;

      --procedure Append

   end Widgets;

end Malef.SDK;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
