-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - W I N D O W S . A D S                     --
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

--
-- @summary
--
--
-- @description
--
package Malef.Windows is

   pragma Elaborate_Body;

   protected type Window_Type is
      procedure Create (Height : Row_Type; Width : Col_Type);
      function Contains (Layer : Malef.Boxes.Layer_Type) return Boolean;
      entry Draw;
      function Get_Position return Coord_Type;
      procedure Insert (Item  : Shared_Surface_Access;
                        Layer : Malef.Boxes.Layer_Type);
      procedure Move (Position : Coord_Type);
      procedure Remove (Layer : Malef.Boxes.Layer_Type);
      procedure Replace (Item  : Shared_Surface_Access;
                         Layer : Malef.Boxes.Layer_Type);
      procedure Resize (Height : Row_Type; Width : Col_Type);
      entry Update;
   private
      procedure Check_Created with Inline;
      Box : Malef.Boxes.Box_Type;
      ID  : Integer := -1;

      Created  : Boolean := False;
      Updating : Boolean := False;
      Drawing  : Boolean := False;
      Resizing : Boolean := False;
   end Window_Type;

   -- type Window_Access is access Window_Type;
   Main_Window : Window_Type;

private

   Last_ID : Natural := 0;

end Malef.Windows;


   -- TODO: XXX: Final thoughts. I'm going to make the Main_Surface a protected
   -- type, because it is the only one that is going to be modified
   -- asynchronously and we can speed up the Put process and the output process
   -- greately. Because we won't have to study the offsets and we can check
   -- which parts of the surface have been modified. Also, as it is a protected
   -- type, we can attach the handler or when calling the resize procedure
   -- it will wait until it's finished doing other things.
   -- About regular surfaces. The will be resizable, but the user will be
   -- warned that it is task safe if they don't resize it. In which case the
   -- user is responsible of it. Because inner functions and procedures won't
   -- modify it's size. No segmentation faults or constraint_errors. The other
   -- problem it may occur is reading at the same time as writing. The read
   -- process may end faster than the writing process. In that case, the user
   -- is responsible for keeping it task safe. Why? Because making everything
   -- a protected type will certantly have performance issues for those who
   -- don't use threads (I imagine the great majority), so it's safer to keep
   -- all the writing and reading operations in the same thread. A new section
   -- will be added to the documentation about good practices.
   -- Other thing to keep in mind is something I read while doing my research
   -- on task safety. I think Gem #99, says something I haven't thought about:
   -- It's that an object could be deallocated twice at the same time if two
   -- tasks check at the same time that the reference counter is zero. Hence,
   -- I'm going TODO add some restrictions.
   --
   -- Other thing to note is the thinking process until I came up with this
   -- idea. I keep the old idea here since it hasn't be commited to keep it as
   -- a record for when I need it again:
   --
   -- -- START OLD DOCUMENTATION --
   -- The Surface_Type has been rethought to be a protected type, that way we
   -- can be sure it is task safe and that it cannot be modified, read and
   -- resized at the same time. It will also be a controlled type, because it
   -- will be freed and destroyed by a wrapper type called Base_Type from which
   -- many other types will inherit.
   --
   -- Before, the Surface_Type had the following problems that should be solved
   -- with this new implementation:
   --
   --  * When the screen was resized it could break lots of things because it
   --    is an asynchronous signal. It could be resized at the same time as
   --    printed onto the screen.
   --
   --  * Other problem were primitive operations, before they were splited in
   --    several packages, now they will be in the same package because it's
   --    more convenient that way.
   --
   --  * Other thing that must be kept in mind is that big programs use several
   --    threads or multitasking.
   -- -- END OLD DOCUMENTATION --



---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
