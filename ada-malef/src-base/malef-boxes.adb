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

   function Contains (Box   : in Box_Type;
                      Layer : Layer_Type)
                      return Boolean is
   begin

      return Box.Layers.Contains (Layer);

   end Contains;


   procedure Insert (Box   : in out Box_Type;
                     Item  : Shared_Surface_Access;
                     Layer : Layer_Type) is
   begin

      if Box.Layers.Contains (Layer) then
         raise Malef.Exceptions.Layer_Error
         with "That layer is already ocupied!";
      end if;

      Box.Layers.Insert (Layer, Layer_Element'(Item, True));
      Reference (Item);

   end Insert;


   procedure Remove (Box   : in out Box_Type;
                     Layer : Layer_Type) is
   begin

      if Box.Layers.Contains (Layer) then
         raise Malef.Exceptions.Layer_Error
         with "Cannot remove non-existent layer!";
      end if;

      Unreference (Box.Layers.Element (Layer).Object);
      Box.Layers.Delete (Layer);

   end Remove;


   procedure Replace (Box   : in out Box_Type;
                      Item  : Shared_Surface_Access;
                      Layer : Layer_Type) is
   begin

      if Box.Layers.Contains (Layer) then
         Box.Layers.Replace (Layer, Layer_Element'(Item, True));
      else
         Box.Layers.Insert (Layer, Layer_Element'(Item, True));
      end if;
      Reference (Item);

   end Replace;


   overriding
   procedure Resize (Box    : in out Box_Type;
                     Height : Row_Type;
                     Width  : Col_Type) is
      Upcast : constant access Base_Type := Base_Type(Box)'Access;
   begin

      if Box.Reference = Shared_Null_Surface'Access then
         Box.Reference := new Shared_Surface_Type;
         Box.Reference.Height   := Shared_Null_Surface.Height;
         Box.Reference.Width    := Shared_Null_Surface.Width;
         Box.Reference.Grid     := new Matrix_Type (1 .. 1, 1 .. 1);
         Box.Reference.Writable := True;
      end if;

      Upcast.Resize (Height, Width);

   end Resize;


   procedure Set_Resizable (Box  : in out Box_Type;
                            Mode : Boolean := True) is
   begin

      Box.Resizable := True;

   end Set_Resizable;


   overriding
   procedure Update (Box : in out Box_Type) is
      Cursor : Object_Maps.Cursor := Box.Layers.First;
      Next   : Layer_Element;
      Base   : Shared_Surface_Access := Box.Reference;

      Top    : Row_Coord := Row_Coord'Last;
      Bottom : Row_Coord := Row_Coord'First;
      Left   : Col_Coord := Col_Coord'Last;
      Right  : Col_Coord := Col_Coord'First;

      Height : Row_Type := 1;
      Width  : Col_Type := 1;

      Row_Offset : Row_Type;
      Col_Offset : Col_Type;

      function Sum (Base, Over : Element_Type) return Element_Type
         with Inline is
      begin
         return Element_Type'(
            Format => Format_Type'(
               Foreground_Color => Base.Format.Foreground_Color +
                                   Over.Format.Foreground_Color,
               Background_Color => Base.Format.Background_Color +
                                   Over.Format.Background_Color,
               Styles           => Over.Format.Styles,
               Attributes       => Over.Format.Attributes
               ),
            Char   => Over.Char
            );
      end Sum;

   -- procedure Put (Color : Color_Type) is
   -- begin
   --    Ada.Text_IO.Put_Line("(" &
   --    Color(R)'Image & "," &
   --    Color(G)'Image & "," &
   --    Color(B)'Image & "," &
   --    Color(A)'Image & " )");
   -- end Put;
   begin

      if Box.Layers.Is_Empty then
         -- Nothing to do.
         return;
      end if;

      -- If it's resizable we first have to get its bounds.
      if Box.Resizable then
         while Object_Maps.Has_Element (Cursor) loop
            Next := Object_Maps.Element (Cursor);

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

            Object_Maps.Next (Cursor);
         end loop;
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

         -- Finally we can paste one surface over the other. We don't need to
         -- check bounds since the surface has been resized to the size of the
         -- layers. It may seem stupid to write twice the same thing, but it's
         -- worth the time saved for calculations.
         while Object_Maps.Has_Element (Cursor) loop
            Next := Object_Maps.Element (Cursor);
            Row_Offset := Row_Type (Next.Object.Position.Row - Top + 1);
            Col_Offset := Col_Type (Next.Object.Position.Col - Left + 1);
            for Row in Next.Object.Grid'Range (1) loop
               for Col in Next.Object.Grid'Range (2) loop
                  Base.Grid (Row + Row_Offset - 1, Col + Col_Offset - 1) :=
                     -- Next.Object.Grid(Row, Col);
                     Sum (Base => Base.Grid (Row + Row_Offset - 1,
                                  Col + Col_Offset - 1),
                          Over => Next.Object.Grid (Row, Col));
               end loop;
            end loop;
            Object_Maps.Next (Cursor);
         end loop;
      else
         Height := Base.Height;
         Width  := Base.Width;
         Top    := Base.Position.Row;
         Bottom := Top + Row_Coord(Height);
         Left   := Base.Position.Col;
         Right  := Left + Col_Coord(Width);
         -- TODO
         while Object_Maps.Has_Element (Cursor) loop
            Next := Object_Maps.Element (Cursor);
            Box.Reference := Next.Object;
            delay 1.0;
            Object_Maps.Next (Cursor);
         end loop;
      end if;

   end Update;


end Malef.Boxes;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
