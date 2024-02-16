-------------------------------------------------------------------------------
--                                                                           --
--            M A L E F - G R O U P S - C O M P O S E R S . A D B            --
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

package body Malef.Groups.Composers with Preelaborate is

   --<<-------->>--
   -->> Adders <<--
   --<<-------->>--

   -->> Simple Addition <<--

   -- First of all is weighting the opacity. If it has the alpha value set
   -- to 1.0 then it's completely opaque and there is no room for the other
   -- base layer (Top) to show up (at least in normal mode).
   -- We also have to keep in mind that we have to sum both alpha values.

   pragma Warnings (Off, "constant ""B_Str"" is not referenced");
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
   pragma Warnings (On, "constant ""B_Str"" is not referenced");

   -- TODO: Remove warnings, GNAT doesn't like (declare) blocks yet
   pragma Warnings (Off, "constant ""W"" is not referenced");
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
   pragma Warnings (On, "constant ""W"" is not referenced");

   -->> Adders <<--

   package Adders is

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

   end Adders;

   --<<----------->>--
   -->> Composers <<--
   --<<----------->>--

   function "*" (Left : in Layer_Opacity; Right : in Component_Type)
      return Component_Type is (
      Component_Type (Float (Left) * Float (Right)));

   function "*" (Left : in Layer_Opacity; Right : in RGBA_Type)
      return RGBA_Type is (
      Right (Red .. Blue) & (Left * Right (Alpha)));

   procedure Compose (
      Object : in out Group;
      Index  : in     Layer_Index;
      Offset : in     Cursor_Type)
   is
      Surface renames Internal_Surface (Object, Index);
      Canvas  renames Object.Surface;
      Opacity  : constant Layer_Opacity := Get_Opacity (Object, Index);
      Position : constant Cursor_Type   := Get_Position (Object, Index);
      Char     : Glyph;
      C_Row    : Positive_Row_Count := Offset.Row + Position.Row + 1;
      C_Col    : Positive_Col_Count;
      Style    : Style_Type;
   begin
      for Row in 1 .. Surface.Rows loop
         C_Col := Offset.Col + Position.Col + 1;
         for Col in 1 .. Surface.Cols loop
            if Canvas (C_Row, C_Col) = Nul then
               Char := Surface (Row, Col);
               Canvas (C_Row, C_Col) := Char;
               Canvas.Set_Foreground (C_Row, C_Col, Adder (
                  Top    => Canvas.Get_Background (C_Row, C_Col),
                  Bottom => Opacity * Surface.Get_Foreground (Row, Col)));
            end if;
            Canvas.Set_Background (C_Row, C_Col, Adder (
               Top    => Canvas.Get_Background (C_Row, C_Col),
               Bottom => Opacity * Surface.Get_Background (Row, Col)));
            Style := Canvas.Get (C_Row, C_Col);
            Style := Style or Surface.Get (Row, Col);
            Canvas.Set (C_Row, C_Col, Style);
            C_Col := C_Col + 1;
         end loop;
         C_Row := C_Row + 1;
      end loop;
   end Compose;

   package Composers is

      procedure None_Composer is new Compose (Adders.Add_None);
      procedure Normal_Composer is new Compose (Adders.Add_Normal);
      procedure Lighten_Composer is new Compose (Adders.Add_Lighten);
      procedure Screen_Composer is new Compose (Adders.Add_Screen);
      procedure Dodge_Composer is new Compose (Adders.Add_Dodge);

   end Composers;

   -->> Getter <<--

   Lookup_Composer : constant array (Layer_Mode) of Composer_Type := [
      None    => Composers.None_Composer'Access,
      Normal  => Composers.Normal_Composer'Access,
      Lighten => Composers.Lighten_Composer'Access,
      Screen  => Composers.Screen_Composer'Access,
      Dodge   => Composers.Dodge_Composer'Access];

   function Get_Composer (
      Mode : in Layer_Mode)
      return Composer_Type is (Lookup_Composer (Mode));

end Malef.Groups.Composers;
