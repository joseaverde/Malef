-------------------------------------------------------------------------------
--                                                                           --
--                       M A L E F - B O X E S . A D B                       --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
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

with Malef.Exceptions;

package body Malef.Boxes is

   function Contains (Box : in Box_Type;
      Layer : Layer_Type)
      return Boolean is
   begin

      return Box.Layers.Contains (Layer);

   end Contains;


   function Get_Layer_Mode (Box : in out Box_Type;
      Layer : Layer_Type)
      return Layer_Mode is
   begin

      if not Box.Layers.Contains (Layer) then
         raise Malef.Exceptions.Layer_Error with
         "Can retrieve layer mode from a layer that does NOT contain any " &
         "surface!";
      end if;

      return Box.Layers.Element (Layer).Mode;

   end Get_Layer_Mode;


   procedure Hide (Box : in out Box_Type;
      Layer : Layer_Type)
   is
      Old_Element : Layer_Element;
   begin

      if not Box.Layers.Contains (Layer) then
         raise Malef.Exceptions.Layer_Error with
         "Can't hide non-existen layer!";
      end if;

      Old_Element := Box.Layers.Element (Layer);
      Old_Element.Hidden := True;
      Box.Layers.Replace (Layer, Old_Element);

   end Hide;


   procedure Insert (Box : in out Box_Type;
      Item  : Surface_Reference;
      Layer : Layer_Type;
      Mode  : Layer_Mode := Normal) is
   begin

      if Box.Layers.Contains (Layer) then
         raise Malef.Exceptions.Layer_Error with
         "That layer is already ocupied!";
      end if;

      Box.Layers.Insert (Layer, Layer_Element'(
         Object => Item.Reference,
         Mode   => Mode,
         others => <>));
      Reference (Item.Reference);

   end Insert;


   function Is_Hidden (Box : in out Box_Type;
      Layer : Layer_Type)
      return Boolean is
   begin

      if not Box.Layers.Contains (Layer) then
         raise Malef.Exceptions.Layer_Error with
         "The given layer does NOT contain any surface!";
      end if;

      return Box.Layers.Element (Layer).Hidden;

   end Is_Hidden;


   function Is_Resizable (Box : in out Box_Type)
      return Boolean is (Box.Resizable);


   procedure Move (Box : in out Box_Type;
      From : Layer_Type;
      To   : Layer_Type)
   is
      Element : Layer_Element;
   begin

      if not Box.Layers.Contains (From) then
         raise Malef.Exceptions.Layer_Error with
         "Can't move a layer which does NOT contain any surface!";
      end if;

      if Box.Layers.Contains (To) then
         raise Malef.Exceptions.Layer_Error with
         "Can't move layer to given position because it already contains a " &
         "surface";
      end if;

      Element := Box.Layers.Element (From);
      Box.Layers.Delete (From);
      Box.Layers.Insert (To, Element);

   end Move;


   procedure Remove (Box : in out Box_Type;
      Layer : Layer_Type) is
   begin

      if not Box.Layers.Contains (Layer) then
         raise Malef.Exceptions.Layer_Error with
         "Cannot remove non-existent layer!";
      end if;

      Unreference (Box.Layers.Element (Layer).Object);
      Box.Layers.Delete (Layer);

   end Remove;


   procedure Replace (Box : in out Box_Type;
      Item  : Surface_Reference;
      Layer : Layer_Type;
      Mode  : Layer_Mode := Normal) is
   begin

      if Box.Layers.Contains (Layer) then
         Box.Layers.Replace (Layer, Layer_Element'(
            Object => Item.Reference,
            Mode   => Mode,
            others => <>));
      else
         Box.Layers.Insert (Layer, Layer_Element'(
            Object => Item.Reference,
            Mode   => Mode,
            others => <>));
      end if;
      Reference (Item.Reference);

   end Replace;


   overriding
   procedure Resize (Box : in out Box_Type;
      Height : Row_Type;
      Width  : Col_Type)
   is
      Upcast : constant access Base_Type := Base_Type(Box)'Access;
   begin

      if Box.Reference = Shared_Null_Surface'Access then
         Box.Reference := new Shared_Surface_Type'(others => <>);
      end if;

      Upcast.Resize (Height, Width);

   end Resize;


   procedure Set_Mode (Box : in out Box_Type;
      Layer : Layer_Type;
      Mode  : Layer_Mode)
   is
      Element : Layer_Element;
   begin

      if not Box.Layers.Contains (Layer) then
         raise Malef.Exceptions.Layer_Error with
         "Can't change layer mode of a layer that does NOT contain any " &
         "surface!";
      end if;

      Element := Box.Layers.Element (Layer);
      Element.Mode := Mode;
      Box.Layers.Replace (Layer, Element);

   end Set_Mode;


   procedure Set_Resizable (Box : in out Box_Type;
      Mode : Boolean := True) is
   begin

      Box.Resizable := Mode;

   end Set_Resizable;


   procedure Unhide (Box : in out Box_Type;
      Layer : Layer_Type)
   is
      Old_Element : Layer_Element;
   begin

      if not Box.Layers.Contains (Layer) then
         raise Malef.Exceptions.Layer_Error with
         "Can't unhide non-existen layer!";
      end if;

      Old_Element := Box.Layers.Element (Layer);
      Old_Element.Hidden := False;
      Box.Layers.Replace (Layer, Old_Element);

   end Unhide;


   overriding
   procedure Update (Box : in out Box_Type;
      Focused : Boolean)
   is
      pragma Unreferenced (Focused);
      -- These variables are used to iterate the items inside the Box.
      Cursor : Object_Maps.Cursor := Box.Layers.First;
      Next   : Layer_Element;
      Base   : Shared_Surface_Access := Box.Reference;

      -- These variables are used to delimite the bounds of the Box' internal
      -- surface. Later, if it's resizable they will be fixed values. If not
      -- the size is guessed from the relative positions of all the surfaces
      -- and their sizes.
      Top    : Row_Coord := Row_Coord'Last;
      Bottom : Row_Coord := Row_Coord'First;
      Left   : Col_Coord := Col_Coord'Last;
      Right  : Col_Coord := Col_Coord'First;

      -- These values are just the height and the width of the Box's internal
      -- surface.
      Height : Row_Type := 1;
      Width  : Col_Type := 1;

      Row_Offset : Row_Type;
      Col_Offset : Col_Type;
      Off_Row    : Row_Type;
      Off_Col    : Col_Type;

      function Sum (Base, Over : Element_Type;
         Mode : Layer_Mode)
         return Element_Type
         with Inline, Pure_Function;

      function Sum (Base, Over : Element_Type;
         Mode : Layer_Mode)
         return Element_Type is
      begin
         return Element : Element_Type
         do
            Element.Format.Styles := Over.Format.Styles;
            Element.Format.Attributes := Over.Format.Attributes;
            Element.Format.Background_Color := Sum (
               Left  => Base.Format.Background_Color,
               Right => Over.Format.Background_Color,
               Mode  => Mode
            );

            if Over.Char <= 32 then
               Element.Format.Foreground_Color := Sum (
                  Left  => Base.Format.Foreground_Color,
                  Right => Over.Format.Background_Color,
                  Mode  => Mode
               );
               Element.Char := Base.Char;
            else
               Element.Format.Foreground_Color := Sum (
                  Left  => Base.Format.Background_Color,
                  Right => Over.Format.Foreground_Color,
                  Mode  => Mode
               );
               Element.Char := Over.Char;
            end if;
         end return;
      end Sum;

   begin

      if Box.Layers.Is_Empty then
         -- Nothing to do.
         return;
      end if;

      if Box.Resizable then
         -- If the surface is resizable we have to calculate all it's bounds.
         while Object_Maps.Has_Element (Cursor) loop
            -- We iterate through all the elements getting their dimensions and
            -- relative positions.
            Next := Object_Maps.Element (Cursor);

            if Next.Hidden then
               goto NEXT_ITEM;
            end if;

            Top    := Row_Coord'Min
               (Top, Next.Object.Position.Row);
            Bottom := Row_Coord'Max
               (Bottom, Next.Object.Position.Row + Row_Coord(
                  Next.Object.Height));
            Left   := Col_Coord'Min
               (Left, Next.Object.Position.Col);
            Right  := Col_Coord'Max
               (Right, Next.Object.Position.Col + Col_Coord(
                  Next.Object.Width));

            <<NEXT_ITEM>>
            Object_Maps.Next (Cursor);
         end loop;
         -- We reset the iterator.
         Cursor := Box.Layers.First;
         Height := Row_Type (Bottom - Top);
         Width  := Col_Type (Right - Left);

         -- We resize the object, if the size is bigger.
         if Height > Box.Reference.Height or Width > Box.Reference.Width then
            Box.Resize (Height, Width);
         else
            Height := Box.Reference.Height;
            Width  := Box.Reference.Width;
         end if;
         Base.Position := (Top, Left);
         Base := Box.Reference;
      else
         -- If it wasn't resizable, the operation is straightforward.
         Height := Base.Height;
         Width  := Base.Width;
         Top    := Base.Position.Row;
         Bottom := Top + Row_Coord(Height);
         Left   := Base.Position.Col;
         Right  := Left + Col_Coord(Width);
      end if;

      -- Finally we can start iterating through all the elements of the Box
      -- putting each one on top of the other. We suppose the Box' Surface to
      -- be Clear. If the user doesn't want to Clear it first, it will still
      -- contains its all contents.
      while Object_Maps.Has_Element (Cursor) loop
         Next := Object_Maps.Element (Cursor);
         if Next.Hidden then
            goto SKIP_PUT_ITEM;
         end if;

         -- We iterate alwasys trying to be in the range of both surfaces.
         -- We have to put the surface inside Next on top of the one inside
         -- Base, i.e: Base.Grid := Base.Grid + Next.Object.Grid
         --
         -- We need to calculate the offset.
         Row_Offset := Row_Type (Next.Object.Position.Row - Top + 1);
         Col_Offset := Col_Type (Next.Object.Position.Col - Left + 1);
         Row_Loop: for Row in Row_Type range Next.Object.Grid'Range (1) loop
            Off_Row := Row + Row_Offset - 1;
            -- If the Offset Row wasn't inside the Box' range we skip it.
            if Off_Row not in Base.Grid'Range (1) then
               goto CONTINUE_ROW_LOOP;
            end if;
            Col_Loop: for Col in Col_Type range Next.Object.Grid'Range (2) loop
               Off_Col := Col + Col_Offset - 1;
               -- Similarly, if the offset Column wasn't in the Box' range we
               -- skip the current iteration.
               if Off_Col not in Base.Grid'Range(2) then
                  goto CONTINUE_COL_LOOP;
               end if;
               Base.Grid (Off_Row, Off_Col) :=
                  Sum (
                     Base => Base.Grid (Off_Row, Off_Col),
                     Over => Next.Object.Grid (Row, Col),
                     Mode => Next.Mode
                  );
               <<CONTINUE_COL_LOOP>>
            end loop Col_Loop;
            <<CONTINUE_ROW_LOOP>>
         end loop Row_Loop;

         <<SKIP_PUT_ITEM>>
         Object_Maps.Next (Cursor);
      end loop;

   end Update;


-- PRIVATE


   function Sum (
      Left  : Color_Type;
      Right : Color_Type;
      Mode  : Layer_Mode := Normal)
      return Color_Type
   is
      --
      -- Since I couldn't find many resources about this equations I'm going
      -- to create my own one just testing with different values.
      -- I took a list of modes from:
      --    <https://docs.gimp.org/en/gimp-concepts-layer-modes.html>
      --
      -- First of all is weighting the opacity. If it has the alpha value set
      -- to 255 then it's completely opaque and there is no room for the other
      -- base layer (Left) to show up (at least in normal mode).
      -- We also have to keep in mind that we have to sum both alpha values.
      --
      Alpha : constant Float := Float'Min (
         255.0, Float(Left(A)) + Float(Right(A)));
      --
      -- Other thing that must be take in mind is that if both Alpha values
      -- sum up 0, there is no point in summing them.
      -- Therefore -- as we can't return in the declaration block -- we can't
      -- declare the Right and Left's strengths as constants.
      --
      Right_Strength, Left_Strength : Float;
      Result : Color_Type;
      subtype RGB is Color_Component_Kind range R .. B;
   begin

      if Alpha = 0.0 then
         -- It's meaningless to perform any operations. Because if we ever sum
         -- it with another layer, it strength will be zero and thus all the
         -- components automatically become zero.
         return (0, 0, 0, 0);
      end if;

      Right_Strength := Float (Right(A)) / Alpha;
      Left_Strength := 1.0 - Right_Strength;

      -- Finally we can SWITCH all the possible modes and return.
      case Mode is
         when None =>
            Result := Right;
         when Normal =>
            -- Result(C) = Right(C)
            for C in RGB'Range loop
               Result (C) := Color_Component_Type (
                  Float(Left(C))  * Left_Strength +
                  Float(Right(C)) * Right_Strength);
            end loop;
         when Lighten =>
            -- Result(C) = Max(Right(C), Left(C));
            for C in RGB'Range loop
               Result (C) := Color_Component_Type'Max (
                  Color_Component_Type (Float(Left(C))  * Left_Strength),
                  Color_Component_Type (Float(Right(C)) * Right_Strength));
            end loop;
         when Screen =>
            -- Result(C) = 255 - (255 - Right(C)) * (255 - Left(C)) / 255
            for C in RGB'Range loop
               Result (C) := 255 - Color_Component_Type (
                  (255.0 - Float(Left(C))  * Left_Strength) *
                  (255.0 - Float(Right(C)) * Right_Strength) / 255.0);
            end loop;
         when Dodge =>
            -- Result(C) = 256 * Left(C) / (255 - Right(C)  + 1)
            for C in RGB'Range loop
               Result (C) := Color_Component_Type (
                  (256.0 * Float(Left(C)) * Left_Strength) /
                  (255.0 - Float(Right(C)) * Right_Strength + 1.0));
            end loop;
         when Addition =>
            -- Result(C) = Min((Left(C)+Right(C)), 255)
            for C in RGB'Range loop
               Result (C) := Color_Component_Type (Float'Min(
                  Left_Strength  * Float(Left(C)) +
                  Right_Strength * Float(Right(C)),
                  255.0));
            end loop;
         -- TODO: Implement the rest of them
         when others => raise Constraint_Error with "Not implemented!";
      end case;
      Result (A) := Color_Component_Type (Alpha);

      return Result;

   end Sum;


   overriding
   procedure Finalize (Box : in out Box_Type) is
      Upcast : constant access Base_Type := Base_Type(Box)'Access;
      Cursor : Object_Maps.Cursor := Box.Layers.First;
      Temp   : Layer_Element;
   begin

      -- Boxes are a special exception of the Base_Type, because there is not
      -- such thing as Null_Box, even though Null_Surfaces exist. At
      -- finalization time, we have to unreference every reference a Box has
      -- inside its layer. The surface is finalised as usual, because it can
      -- be found in other datatypes including in itself, so about finalising
      -- the Surface, there is no problem.
      --
      -- This has been the cause of an important memory leak. So, to address
      -- this problem we first treat a Box as any other type derived from the
      -- Base_Type, by upcasting it. Because even if it frees the surface, it
      -- won't free itself, because a Box is just a statically allocated
      -- record.
      --
      -- We first finalize the internal shared surface. We don't care if it
      -- freed or not, because we are going to unreference every surface in any
      -- case.
      Upcast.Finalize;

      while Object_Maps.Has_Element (Cursor) loop
         Temp := Object_Maps.Element (Cursor);
         -- We finalize them one by one.
         Unreference (Temp.Object);
         -- If for any given reason the last reference it had was in this very
         -- same box, we must remove it from it to avoid any kind of corruption
         if Temp.Object = Shared_Null_Surface'Access then
            Box.Layers.Delete (Cursor);
         end if;
         Object_Maps.Next (Cursor);
      end loop;

   end Finalize;


   overriding
   procedure Adjust (Box : in out Box_Type) is
      Upcast : constant access Base_Type := Base_Type(Box)'Access;
      Cursor : Object_Maps.Cursor := Box.Layers.First;
   begin

      -- You can read the comments in the function Finalize for more
      -- information. But basically, we have to increase the references of the
      -- objects referenced by the original box.
      Upcast.Adjust;

      while Object_Maps.Has_Element (Cursor) loop
         Reference (Object_Maps.Element (Cursor).Object);
         Object_Maps.Next (Cursor);
      end loop;

   end Adjust;


end Malef.Boxes;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
