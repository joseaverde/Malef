-------------------------------------------------------------------------------
--                                                                           --
--                       M A L E F - B O X E S . A D S                       --
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

private with Ada.Containers.Ordered_Maps;

--
-- @summary
-- Boxes are a datatype you can use to store multiple Surface in one place. You
-- insert them and when you update the box, all the changes made are updated
-- inside the Box's internal Surface. You can't modify a Box as a Surface, but
-- as it inherits from the same Parent type (Base_Type), both of them can be
-- used in similar environments.
-- Just imagine a box as a surface that shows how the Surfaces would be seen
-- if they were put one into each other.
-- To put a new object into the Box use the .Get_Reference method.
--
-- @description
-- The boxes's surfaces can be resized automatically so all Surfaces fit on it,
-- this options is way faster than using a fixed size Box. But you can disable
-- this behaviour yourself using the Set_Resizable procedure.
-- Keep in mind that you can put Boxes inside other Boxes, but if you update
-- a higher Box, it won't update the other boxes. This is done to avoid
-- recursion problems.
-- Keep also in mind that you have to clear the box from time to time, because
-- by default the background isn't cleared at update time.
-- Also, if you have set your Box to be resizable, it will use the relative
-- positions of the surfaces. So if any surface is found in a negative
-- coordinate point, the position of the surface will be it, and it will be
-- stretched to fit all the Surfaces.
-- WARNING: The Box_Type isn't task safe, if you want a task-safe variant use
--   Malef.Windows.Window_Type instead. Boxes are safe if you don't update it
--   and write it as the same time. If you have a more complex library with
--   tasks use the Window_Type instead. The only problem Windows have is that
--   you cannot get their references. For example, if you have created your Box
--   and added your surfaces and updated it If you disable resizing, there
--   should be no problem using them (even if resizing is enabled). The problem
--   arises when you Draw, Read, Update and those things at the same time. If
--   you keep your Box in one thread even if it's a multithreaded program, you
--   shouldn't experience any problem.
--
package Malef.Boxes is

   --
   -- The layer type is just an Integer you can use to set the position of any
   -- given Surface inside a Box. For instance, if you have a Surface_A on
   -- Layer 1 and Surface_B on Layer 2: then Surface_A will be drawn before
   -- Surface_B, thus Surface_B will be written above Surface_A. You don't have
   -- to follow any strict order, you can jump from Layer 1 to Layer 2000,
   -- however, two Surfaces CANNOT be put onto the same Layer.
   --
   type Layer_Type is new Positive;


   --
   -- The layer modes are based on the GIMP ones:
   --    <https://docs.gimp.org/en/gimp-concepts-layer-modes.html>
   type Layer_Mode is (
      None,
      Normal,
      Lighten,
      Screen,
      Dodge,
      Addition,
      Darken,
      Multiply,
      Burn,
      Overlay,
      Soft_Light,
      Hard_Light,
      Difference,
      Substract,
      Grain_Extract,
      Grain_Merge,
      Divide
   );

   --
   -- This is the Box_Type, as the description of this packages says, the
   -- Box_Type is just a Container where you can store Surfaces, or other
   -- datastructures that inherit from Base_Type, such as Boxes themselves.
   -- This type doesn't require any kind of initialisation, it can be used as
   -- it is, but don't forget to Update it before using it.
   --
   type Box_Type is new Base_Type with private;


   --
   -- @return
   -- This function returns whether a Layer is already being in use or not.
   --
   -- @param Layer
   -- The layer in question.
   --
   function Contains (Box : in Box_Type;
      Layer : Layer_Type)
      return Boolean;


   --
   -- @return
   -- This function returns the layer blending mode of a given layer.
   --
   -- @exception Malef.Exceptions.Layer_Error
   -- This exception is raised when the layer doesn't contain any surface.
   --
   function Get_Layer_Mode (Box : in out Box_Type;
      Layer : Layer_Type)
      return Layer_Mode;


   --
   -- This function hides a layer.
   --
   -- @param Layer
   -- The layer to hide.
   --
   -- @exception Malef.Exceptions.Layer_Error
   -- This exception is raised when the layer doesn't contain any surface.
   --
   procedure Hide (Box : in out Box_Type;
      Layer : Layer_Type);


   -- This procedure inserts an Item in the given Layer position.
   --
   -- @param Item
   -- The item to put. You just have to used the .Get_Reference method on any
   -- type derived from the Base_Type.
   --
   -- @param Layer
   -- The Layer to put the Item on.
   --
   -- @exception Malef.Exceptions.Layer_Error
   -- This exception is raised when you are trying to insert an object into a
   -- Layer that already has an item.
   --
   procedure Insert (Box : in out Box_Type;
      Item  : Surface_Reference;
      Layer : Layer_Type;
      Mode  : Layer_Mode := Normal);

   --
   -- @return
   -- It returns whether a given layer is hidden or not.
   --
   -- @exception Malef.Exceptions.Layer_Error
   -- When the given layer doesn't contain any surface.
   --
   function Is_Hidden (Box : in out Box_Type;
      Layer : Layer_Type)
      return Boolean;


   --
   -- @return
   -- Returns whether the box' internal surface is resizable.
   --
   function Is_Resizable (Box : in out Box_Type)
      return Boolean
      with Inline;


   --
   -- This procedure changes the surface from a layer to another EMPTY one.
   --
   -- @exception Malef.Exceptions.Layer_Error
   -- This exceptions is raised when either the From layer does NOT contain
   -- any surface or the To layer DOES contain a Surface (It's not EMPTY).
   --
   procedure Move (Box : in out Box_Type;
      From : Layer_Type;
      To   : Layer_Type);


   --
   -- This procedure removes a layer from the box.
   --
   -- @param Layer
   -- The layer to remove the element from.
   --
   -- @param Mode
   -- The layer blending mode.
   --
   -- @exception Malef.Exceptions.Layer_Error
   -- This exception is raised when the user is trying to remove a layer that
   -- doesn't contain any kind of object.
   --
   procedure Remove (Box : in out Box_Type;
      Layer : Layer_Type);

   --
   -- This procedure works as the Insert procedure but without needing to worry
   -- about whether it already contains an element. It doesn't raise any kind
   -- of exception. If it's free it inserts it there, otherwise it replaces it.
   --
   -- @param Item
   -- The item you wan to insert into the Box.
   --
   -- @param Layer
   -- The layer where you want it to be.
   --
   -- @param Mode
   -- The layer blending mode.
   --
   procedure Replace (Box : in out Box_Type;
      Item  : Surface_Reference;
      Layer : Layer_Type;
      Mode  : Layer_Mode := Normal);

   --
   -- This function should be called under your own risk. This procedure
   -- changes the size of a Box to a new one. In single threaded applications
   -- it won't be a problem, but in multithreaded ones you have to keep it in
   -- mind. That's the reason the Window_Type was created, so it task safe and
   -- which also behaves in the safe way as a Box.
   -- Also, keep in mind this function is called when Updating.
   --
   -- @param Height
   -- The new Height of the Box.
   --
   -- @param Width
   -- The new Width of the Box.
   --
   -- @exception Malef.Exceptions.Layer_Error
   -- It doesn't raise any exception even though it is raised in with other
   -- types, because even if the internal surface contained by the Box is by
   -- default a Shared_Null_Surface, it will allocate a new one for the box.
   --
   overriding
   procedure Resize (Box : in out Box_Type;
      Height : Row_Type;
      Width  : Col_Type);


   --
   -- This procedure changes the blending mode of a layer.
   --
   -- @exception Malef.Exceptions.Layer_Error
   -- This exception is raised when the layer doesn't contain any surface.
   --
   procedure Set_Mode (Box : in out Box_Type;
      Layer : Layer_Type;
      Mode  : Layer_Mode);

   --
   -- By default, Boxes change their sizes when updated to fit the Surfaces it
   -- contains. If you want to disable this behaviour use this function.
   -- However, the process may be faster if you allow it to be resized.
   --
   -- @param Mode
   -- Resizable or not?
   --
   procedure Set_Resizable (Box : in out Box_Type;
      Mode : Boolean := True);


   --
   -- This procedure unhides a given layer from the box.
   --
   -- @exception Malef.Exceptions.Layer_Error
   -- When the given layer doesn't contain any surface.
   --
   procedure Unhide (Box : in out Box_Type;
      Layer : Layer_Type);

   --
   -- This function only makes sense for the Box type. When updating it copies
   -- the contents of the other surfaces using the different modes to create a
   -- single surface with the contents of every other surface. This process may
   -- alter the size of your box if you hadn't disabled it to be Resizable. In
   -- any case, during the Update process, the Box shouldn't be Resizing.
   --
   overriding
   procedure Update (Box : in out Box_Type;
      Focused : Boolean);

private

   function Sum (
      Left  : Color_Type;
      Right : Color_Type;
      Mode  : Layer_Mode := Normal)
      return Color_Type
      with Inline, Pure_Function;

   type Layer_Element is
      record
         Object : Shared_Surface_Access := Shared_Null_Surface'Access;
         Mode   : Layer_Mode := Normal;
         Hidden : Boolean  := False;
         Is_New : Boolean  := True;
      end record;

   package Object_Maps is new Ada.Containers.Ordered_Maps (
      Key_Type     => Layer_Type,
      Element_Type => Layer_Element
   );
   type Box_Type is new Base_Type with
      record
         Layers    : Object_Maps.Map;
         Resizable : Boolean := True;
      end record;

   -- We have to unreference everything.
   overriding
   procedure Finalize (Box : in out Box_Type);

   overriding
   procedure Adjust (Box : in out Box_Type);

end Malef.Boxes;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
