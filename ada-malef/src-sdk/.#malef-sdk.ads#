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
-- This package is aimed to provide a simple way to create interactive
-- applications with Malef. You can create your own Widgets inheriting from
-- the Widget_Engine_Type. A better description is found bellow.
--
-- @description
-- This package defines two types:
--  * Widget_Type: Which is private. When creating your Widgets you have to
--  define a function (usually called Create) that returns a Widget_Type
--  containing a dynamically allocated instance of your new type. That way you
--  can be sure that the Widget will be deallocated when the scope terminates.
--
--  * Widget_Engine_Type; This is the type you can extend. By default it
--  contains a Box, a Surface and another Surface for a Shadow. This
--  objects default to null. You have to override the abstract functions and
--  any other if you want.
--
-- How does it work?
--  For a better description of each of the parts read the documentation of
-- each of the functions. But the thought process behind this is:
--
-- Imagine you want to create the classical program with the Top Bar (File,
-- Options, About...) and bellow some kind of program (A word processor,
-- graphs...). You can move through all the menus (File, Options...) and
-- submenus (File -> Open -> Select File) allowing other non-focused menus
-- (such as the graphs) to update while you do other things.
--
-- To simplify the process of creating interactive applications Malef declares
-- different Widgets:
-- * Multi Widgets : A Widget which contains a set of different Widgets that
-- are updated at the same time.
-- * Function Widget : A Widget which runs a procedure or a function.
-- * String Widgets : This Widget is for getting user input
-- * Message Widget: This Widget is like menus but contain a big message.
-- * Menu Widgets: Menu Widgets.
-- * Button Widget : Buttons
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
         end record;

      type Any_Widget_Access is not null access Widget_Type'Class;

      overriding
      function Get_Reference (Widget : in Widget_Type)
         return Surface_Reference;

      --
      -- This function when runned tells the user
      function Run (Widget : in out Widget_Type)
         return Return_Type is abstract;

   end Widgets;

-- type Widget_Type is tagged private;

-- type Widget_Engine_Type is abstract new Base_Type with
--    record
--       Box      : Malef.Boxes.Box_Type;
--       Surface  : Malef.Surfaces.Surface_Type;
--       Shadow   : Malef.Surfaces.Surface_Type;
--    end record;
-- type Any_Widget is access Widget_Engine_Type;

-- type Any_Widget_Access_Array is array (Return_Type'Range) of

-- overriding
-- function Get_Reference (Widget : in Widget_Engine_Type)
--    return Surface_Reference;

-- overriding
-- function Update (Widget : in out Widget_Engine_Type;
--    Focused : in Boolean)

-- procedure Assign (Widget : in out Widget_Type;
--    Position : Return_Type;
--    Element  : Any_Widget_Access);

end Malef.SDK;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
