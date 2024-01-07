-------------------------------------------------------------------------------
--                                                                           --
--                       M A L E F - B O X E S . A D B                       --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   B O D Y                              --
--                                                                           --
-------------------------------------------------------------------------------
-- Copyright (c) 2021 - 2024  José Antonio Verde Jiménez All Rights Reserved --
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

package body Malef.Boxes is

   procedure Set_Updated (
      Object : in out Box;
      Layer  : in     Layer_Index)
   is
      pragma Unreferenced (Layer);
   begin
      -- TODO: Apply optimisations
      Object.Updated := True;
   end Set_Updated;

   --<<------------->>--
   -->> Layer Modes <<--
   --<<------------->>--

   function "*" (Left : in Layer_Opacity; Right : in Component_Type)
      return Component_Type is (
      Component_Type (Float (Left) * Float (Right)));

   function "*" (Left : in Layer_Opacity; Right : in RGBA_Type)
      return RGBA_Type is (
      Right (Red .. Blue) & (Left * Right (Alpha)));

   package Layer_Modes_Helper is

      generic
         with function Add (Top, Bottom : in Float) return Float;
      function Simple_Addition (Top, Bottom : in RGBA_Type)
         return RGBA_Type;

      type Weights is
         record
            Alpha  : Component_Type;
            Top    : Float;
            Bottom : Float;
         end record;
      -- First of all is weighting the opacity. If it has the alpha value set
      -- to 1.0 then it's completely opaque and there is no room for the other
      -- base layer (Top) to show up (at least in normal mode).
      -- We also have to keep in mind that we have to sum both alpha values.

      function Weight (Top, Bottom : in RGBA_Type)
         return Weights is ((
         declare
             Other : constant Component_Type
                   := Component_Type'Min (Bottom (Alpha),
                                          Component_Type'Last - Top (Alpha));
             Total : constant Component_Type := Top (Alpha) + Other;
             T_Str : constant Float := Float (Top (Alpha)) / Float (Total);
             B_Str : constant Float := 1.0 - T_Str;
         begin
            (Total, T_Str, B_Str)));

   end Layer_Modes_Helper;

   package body Layer_Modes_Helper is

      function Simple_Addition (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (
         (if    Top (Alpha) = 255 then Top
          elsif Top (Alpha) = 0   then Bottom
          else (declare
            W : constant Weights := Weight (Top, Bottom);
          begin
             [for I in Top'Range =>
                (if I = Alpha then W.Alpha
                  else Component_Type (Add (Float (Top (I)) * W.Top,
                                            Float (Bottom (I)) * W.Bottom)))])
         ));

   end Layer_Modes_Helper;

   package Layer_Modes is

      type Layer_Addition is not null
         access function (Top, Bottom : in RGBA_Type) return RGBA_Type;

      type Addition_Array is array (Layer_Mode) of Layer_Addition;

      Selector : constant Addition_Array;

   private

      use Layer_Modes_Helper;

      -- Since I couldn't find many resources about these equations I'm going
      -- to create my own just testing with different values.
      -- I took a list of modes from:
      --    <https://docs.gimp.org/en/gimp-concepts-layer-modes.html>

      function Add_None (Top, Bottom : in RGBA_Type) return RGBA_Type is
         ((if Top (Alpha) /= 0 then Top else Bottom));

      function Add_Normal is new Simple_Addition ("+");

      function Add_Lighten is new Simple_Addition (Float'Max);

      function Screen_Function (Left, Right : in Float) return Float is (
         255.0 - ((255.0 - Left) * (255.0 * Right) / 255.0));
      function Add_Screen is new Simple_Addition (Screen_Function);

      function Dodge_Function (Left, Right : in Float) return Float is (
         256.0 * Left / (255.0 - Right + 1.0));
      function Add_Dodge is new Simple_Addition (Dodge_Function);

      function Add_Addition (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (Add_None (Top, Bottom));

      function Add_Darken (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (Add_None (Top, Bottom));

      function Add_Multiply (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (Add_None (Top, Bottom));

      function Add_Burn (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (Add_None (Top, Bottom));

      function Add_Overlay (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (Add_None (Top, Bottom));

      function Add_Soft_Light (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (Add_None (Top, Bottom));

      function Add_Hard_Light (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (Add_None (Top, Bottom));

      function Add_Difference (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (Add_None (Top, Bottom));

      function Add_Substract (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (Add_None (Top, Bottom));

      function Add_Grain_Extract (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (Add_None (Top, Bottom));

      function Add_Grain_Merge (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (Add_None (Top, Bottom));

      function Add_Divide (Top, Bottom : in RGBA_Type)
         return RGBA_Type is (Add_None (Top, Bottom));

      Selector : constant Addition_Array := (
         None          => Add_None'Access,
         Normal        => Add_Normal'Access,
         Lighten       => Add_Lighten'Access,
         Screen        => Add_Screen'Access,
         Dodge         => Add_Dodge'Access,
         Addition      => Add_Addition'Access,
         Darken        => Add_Darken'Access,
         Multiply      => Add_Multiply'Access,
         Burn          => Add_Burn'Access,
         Overlay       => Add_Overlay'Access,
         Soft_Light    => Add_Soft_Light'Access,
         Hard_Light    => Add_Hard_Light'Access,
         Difference    => Add_Difference'Access,
         Substract     => Add_Substract'Access,
         Grain_Extract => Add_Grain_Extract'Access,
         Grain_Merge   => Add_Grain_Merge'Access,
         Divide        => Add_Divide'Access);

   end Layer_Modes;

   --<<---------------->>--
   -->> Box Operations <<--
   --<<---------------->>--

   function Increase_Size (
      Object : in out Box;
      Shift  :    out Cursor_Type)
      return Boolean
   is
      Min : Cursor_Type := (Row_Type'Last, Col_Type'Last);
      Max : Cursor_Type := (Row_Type'First, Col_Type'First);
      Pos : Cursor_Type;
      Siz : Cursor_Type;
   begin
      for I in 1 .. Object.Capacity when Object.Contains (I) loop
         Pos := Object.Get_Position (I);
         Siz := (Row => Object.Layers (I).Surface.all.Rows,
                 Col => Object.Layers (I).Surface.all.Cols);
         Min := Cursor_Type'(Row => Row_Type'Min (Min.Row, Pos.Row),
                             Col => Col_Type'Min (Min.Col, Pos.Col));
         Max := Cursor_Type'(Row => Row_Type'Max (Max.Row, Siz.Row + Pos.Row),
                             Col => Col_Type'Max (Max.Col, Siz.Col + Pos.Col));
      end loop;
      if Max.Col < Min.Col or else Max.Row < Min.Row then
         return False;
      end if;
      Shift := (1 - Min.Row, 1 - Min.Col);
      Siz := (Max.Row - Min.Row + 1, Max.Col - Min.Col + 1);
      if Object.Canvas.Is_Empty                          or else
         Object.Canvas.Constant_Reference.Rows < Siz.Row or else
         Object.Canvas.Constant_Reference.Cols < Siz.Col
      then
         Object.Canvas.Clear;
         declare
            Surface : Surfaces.Surface (Siz.Row, Siz.Col);
         begin
            Object.Canvas.Replace_Element (Surface);
         end;
      end if;
      return True;
   end Increase_Size;

   procedure Clear_Surface (
      Object : in out Box) is
   begin
      Object.Canvas.Reference.Fill ((0, 0, 0, 0), (0, 0, 0, 0), Nil, No_Style);
   end Clear_Surface;

   function Get_Surface (
      Object : in Box;
      Index  : in Layer_Index)
      return not null access Surfaces.Surface is (
      (if Object.Layers (Index).Surface /= null
         then Object.Layers (Index).Surface
         else Object.Layers (Index).Box.Canvas.Reference.Element));

   procedure Add_Layer (
      Object : in out Box;
      Index  : in     Layer_Index;
      Shift  : in     Cursor_Type;
      Add    : in     Layer_Modes.Layer_Addition)
   is
      Surface renames Get_Surface (Object, Index);
      Canvas renames Object.Canvas.Reference.Element;
      Opacity : constant Layer_Opacity := Object.Get_Opacity (Index);
      Pos     : constant Cursor_Type := Object.Get_Position (Index);
      Char    : Glyph;
      C_Row   : Positive_Row_Count := Shift.Row + Pos.Row + 1;
      C_Col   : Positive_Col_Count;
   begin
      for Row in 1 .. Surface.Rows loop
         C_Col := Shift.Col + Pos.Col + 1;
         for Col in 1 .. Surface.Cols loop
            if Canvas (C_Row, C_Col) = Nil then
               Char := Surface (Row, Col);
               Canvas (C_Row, C_Col) := Char;
               Canvas.Set_Foreground (C_Row, C_Col, Add.all (
                  Top    => Canvas.Get_Background (C_Row, C_Col),
                  Bottom => Opacity * Surface.Get_Foreground (Row, Col)));
            end if;
            Canvas.Set_Background (C_Row, C_Col, Add.all (
               Top    => Canvas.Get_Background (C_Row, C_Col),
               Bottom => Opacity * Surface.Get_Background (Row, Col)));
            C_Col := C_Col + 1;
         end loop;
         C_Row := C_Row + 1;
      end loop;
   end Add_Layer;

   function Mark_Updated (
      Object : in out Box)
      return Boolean
   is
      Count : Natural := 0;
   begin
      -- TODO: Add optimisation to avoid redrawing a lot.
      for I in 1 .. Object.Capacity when Object.Contains (I) loop
         if Object.Layers (I).Surface /= null and then
            Object.Layers (I).Surface.Modified
         then
            Object.Layers (I).Surface.Set_Up_to_Date;
            Count := Count + 1;
         elsif Object.Layers (I).Box /= null and then
               Object.Layers (I).Box.Updated
         then
            Object.Layers (I).Box.Update;
            Count := Count + 1;
         end if;
      end loop;
      return Count > 0;
   end Mark_Updated;

   procedure Update (
      Object : in out Box)
   is
      -- TODO: Add Optimizations with Set_Updated and draw only what's needed.
      Add   : Layer_Modes.Layer_Addition := Layer_Modes.Selector (Normal);
      Shift : Cursor_Type;
   begin
      Object.Updated := True;
      if not Object.Updated                or else
         not Mark_Updated (Object)         or else
         not Increase_Size (Object, Shift)
      then
         return;
      end if;
      Clear_Surface (Object);
      for I in reverse 1 .. Object.Capacity
         when Object.Contains (I)
         and then (if Object.Layers (I).Box /= null
                     then not Object.Layers (I).Box.Canvas.Is_Empty)
      loop
         Add_Layer (Object, I, Shift, Add);
         Add := Layer_Modes.Selector (Get_Mode (Object, I));
      end loop;
   end Update;

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Box) is
   begin
      Buffer.Put ("BOX ["); Buffer.Put (Arg.Count'Image);
      Buffer.Put ("/");     Buffer.Put (Arg.Capacity'Image);
      Buffer.Put (" ]");
      if Arg.Canvas.Is_Empty then
         Buffer.Put (" is not updated ");
      else
         Buffer.Put (Arg.Canvas.Constant_Reference.Element.all'Image);
      end if;
   end Put_Image;

   --<<---------------------->>--
   -->> Container Operations <<--
   --<<---------------------->>--

   procedure Insert (
      Object   : in out Box;
      Index    : in     Layer_Index;
      Surface  : in     not null Surface_Access;
      Position : in     Cursor_Type   := (1, 1);
      Mode     : in     Layer_Mode    := Normal;
      Opacity  : in     Layer_Opacity := 1.0) is
   begin
      Object.Layers (Index) := (Surface  => Surface,
                                Box      => null,
                                Opacity  => Opacity,
                                Position => Position,
                                Mode     => Mode,
                                Hidden   => False,
                                In_Use   => True);
      Object.Count := @ + 1;
      Set_Updated (Object, Index);
   end Insert;

   procedure Insert (
      Object   : in out Box;
      Index    : in     Layer_Index;
      Other    : in     not null Box_Access;
      Position : in     Cursor_Type   := (1, 1);
      Mode     : in     Layer_Mode    := Normal;
      Opacity  : in     Layer_Opacity := 1.0) is
   begin
      Object.Layers (Index) := (Surface  => null,
                                Box      => Other,
                                Opacity  => Opacity,
                                Position => Position,
                                Mode     => Mode,
                                Hidden   => False,
                                In_Use   => True);
      Object.Count := @ + 1;
      Set_Updated (Object, Index);
   end Insert;

   procedure Delete (
      Object : in out Box;
      Index  : in     Layer_Index) is
   begin
      Object.Layers (Index).In_Use := False;
      Object.Layers (Index).Surface := null;
      Object.Layers (Index).Box := null;
      Object.Count := @ - 1;
      Set_Updated (Object, Index);
   end Delete;

   --<<------------------>>--
   -->> Layer Operations <<--
   --<<------------------>>--

   procedure Hide (
      Object : in out Box;
      Index  : in     Layer_Index) is
   begin
      Object.Layers (Index).Hidden := True;
      Set_Updated (Object, Index);
   end Hide;

   procedure Show (
      Object : in out Box;
      Index  : in     Layer_Index) is
   begin
      Object.Layers (Index).Hidden := False;
      Set_Updated (Object, Index);
   end Show;

   procedure Set_Mode (
      Object : in out Box;
      Index  : in     Layer_Index;
      Item   : in     Layer_Mode) is
   begin
      Object.Layers (Index).Mode := Item;
      Set_Updated (Object, Index);
   end Set_Mode;

   procedure Set_Opacity (
      Object : in out Box;
      Index  : in     Layer_Index;
      Item   : in     Layer_Opacity) is
   begin
      Object.Layers (Index).Opacity := Item;
      Set_Updated (Object, Index);
   end Set_Opacity;

   procedure Move_Layer (
      Object : in out Box;
      Index  : in Layer_Index;
      To     : in     Cursor_Type) is
   begin
      Set_Updated (Object, Index);
      Object.Layers (Index).Position := To;
      Set_Updated (Object, Index);
   end Move_Layer;

end Malef.Boxes;
