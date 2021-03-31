-------------------------------------------------------------------------------
--                                                                           --
--                    C _ M A L E F - C O L O R S . A D B                    --
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

with C_Malef.Errors;
with Malef.Exceptions;


package body C_Malef.Colors is

   function Get_Foreground (Surface :    Surface_Type;
                            Row     :     Row_Type;
                            Col     :     Col_Type;
                            Color   : out Color_Type)
                            return Error_Kind is
   begin

      Color := To_C(Malef.Colors.Get_Foreground(
                     Surface => Surface.Object,
                     Row     => Malef.Row_Type(Row),
                     Col     => Malef.Col_Type(Col)));

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Bounds_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Bounds_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Get_Foreground;


   function Get_Background (Surface :     Surface_Type;
                            Row     :     Row_Type;
                            Col     :     Col_Type;
                            Color   : out Color_Type)
                            return Error_Kind is
   begin

      Color := To_C(Malef.Colors.Get_Background(
                     Surface => Surface.Object,
                     Row     => Malef.Row_Type(Row),
                     Col     => Malef.Col_Type(Col)));

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Bounds_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Bounds_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Get_Background;



   function Set_Foreground (Surface  : Surface_Type;
                            From_Row : Row_Type;
                            To_Row   : Row_Type;
                            From_Col : Col_Type;
                            To_Col   : Col_Type;
                            Color    : Color_Type)
                            return Error_Kind is
   begin

      Malef.Colors.Set_Foreground(Surface  => Surface.Object,
                                  From_Row => Malef.Row_Type(From_Row),
                                  To_Row   => Malef.Row_Type(To_Row),
                                  From_Col => Malef.Col_Type(From_Col),
                                  To_Col   => Malef.Col_Type(To_Col),
                                  Color    => To_Ada(Color));

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Bounds_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Bounds_Error;
      when Ada_Exception : Malef.Exceptions.Null_Surface_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Null_Surface_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Set_Foreground;


   function Set_Background (Surface  : Surface_Type;
                             From_Row : Row_Type;
                             To_Row   : Row_Type;
                             From_Col : Col_Type;
                             To_Col   : Col_Type;
                             Color    : Color_Type)
                             return Error_Kind is
   begin

      Malef.Colors.Set_Background(Surface  => Surface.Object,
                                  From_Row => Malef.Row_Type(From_Row),
                                  To_Row   => Malef.Row_Type(To_Row),
                                  From_Col => Malef.Col_Type(From_Col),
                                  To_Col   => Malef.Col_Type(To_Col),
                                  Color    => To_Ada(Color));

      return No_Error;

   exception
      when Ada_Exception : Malef.Exceptions.Bounds_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Bounds_Error;
      when Ada_Exception : Malef.Exceptions.Null_Surface_Error =>
         C_Malef.Errors.Push(Ada_Exception);
         return Null_Surface_Error;
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;

   end Set_Background;




   function Get_Cursor_Foreground (Surface :     Surface_Type;
                                   Color   : out Color_Type)
                                   return Error_Kind is
   begin

      Color := To_C(Malef.Colors.Get_Cursor_Foreground(Surface.Object));

      return No_Error;

   exception
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Get_Cursor_Foreground;


   function Get_Cursor_Background (Surface :     Surface_Type;
                                   Color   : out Color_Type)
                                   return Error_Kind is
   begin

      Color := To_C(Malef.Colors.Get_Cursor_Background(Surface.Object));

      return No_Error;

   exception
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Get_Cursor_Background;



   function Set_Cursor_Foreground (Surface : Surface_Type;
                                   Color   : Color_Type)
                                   return Error_Kind is
   begin

      Malef.Colors.Set_Cursor_Foreground(Surface => Surface.Object,
                                         Color   => To_Ada(Color));

      return No_Error;

   exception
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Set_Cursor_Foreground;


   function Set_Cursor_Background (Surface : Surface_Type;
                                   Color   : Color_Type)
                                   return Error_Kind is
   begin

      Malef.Colors.Set_Cursor_Background(Surface => Surface.Object,
                                         Color   => To_Ada(Color));

      return No_Error;

   exception
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Set_Cursor_Background;



   function Get_Palette (Palette : out Palette_Type)
                         return Error_Kind is
   begin

      Palette := To_C(Malef.Colors.Get_Palette);

      return No_Error;

   exception
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Get_Palette;


   function Get_Palette (Kind    :     Palette_Kind;
                         Palette : out Palette_Type)
                         return Error_Kind is
   begin

      Palette := To_C(Malef.Colors.Palettes(To_Ada(Kind)));

      return No_Error;

   exception
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Get_Palette;


   function Set_Palette (Palette : Palette_Type)
                         return Error_Kind is
   begin

      Malef.Colors.Set_Palette(To_Ada(Palette));

      return No_Error;

   exception
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Set_Palette;


   function Set_Palette (Kind : Palette_Kind)
                         return Error_Kind is
   begin

      Malef.Colors.Set_Palette(To_Ada(Kind));

      return No_Error;

   exception
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Set_Palette;



   function Get_Color (Color  : out Color_Type;
                       Kind   : in  Color_Kind;
                       Bright : in  bool)
                       return Error_Kind is
   begin

      Color := To_C(Malef.Colors.Get_Color(
         Kind   => To_Ada(Kind),
         Bright => To_Ada(Bright)));

      return No_Error;

   exception
      when Ada_Exception : others =>
         C_Malef.Errors.Push(Ada_Exception);
         return Ada_Error;
   end Get_Color;


-- PRIVATE --

   function To_Ada (Item : Color_Type)
                    return Malef.Color_Type is
   begin

      return New_Color : Malef.Color_Type
      do
         New_Color := (Malef.R => Malef.Color_Component_Type(Item(R)),
                       Malef.G => Malef.Color_Component_Type(Item(G)),
                       Malef.B => Malef.Color_Component_Type(Item(B)),
                       Malef.A => Malef.Color_Component_Type(Item(A)));
      end return;

   end To_Ada;


   function To_C (Item : Malef.Color_Type)
                  return Color_Type is
   begin

      return New_Color : Color_Type
      do
         New_Color := (R => C_Malef.Color_Component_Type(Item(Malef.R)),
                       G => C_Malef.Color_Component_Type(Item(Malef.G)),
                       B => C_Malef.Color_Component_Type(Item(Malef.B)),
                       A => C_Malef.Color_Component_Type(Item(Malef.A)));
      end return;

   end To_C;



   function To_Ada (Item : Palette_Type)
                    return Malef.Colors.Palette_Type is
      lastBool  : bool;
   begin

      return New_Palette : Malef.Colors.Palette_Type
      do
         for B in New_Palette'Range(1) loop
            lastBool := To_C (B);
            for C in New_Palette'Range(2) loop
               New_Palette(B, C) := To_Ada(Item(lastBool, To_C(C)));
            end loop;
         end loop;
      end return;

   end To_Ada;


   function To_C (Item : Malef.Colors.Palette_Type)
                    return Palette_Type is
      Last_Boolean : Boolean;
   begin

      return New_Palette : Palette_Type
      do
         for B in New_Palette'Range(1) loop
            Last_Boolean := To_Ada(B);
            for C in New_Palette'Range(2) loop
               New_Palette(B, C) := To_C(Item(Last_Boolean,To_Ada(C)));
            end loop;
         end loop;
      end return;

   end To_C;


   paletteKindAda2C : constant array (Malef.Colors.Palette_Kind'Range)
                               of Palette_Kind :=
      (Malef.Colors.Malef_Palette      => Malef_Palette,
       Malef.Colors.VGA                => VGA,
       Malef.Colors.Windows_XP_Console => Windows_XP_Console,
       Malef.Colors.Windows_PowerShell => Windows_PowerShell,
       Malef.Colors.Visual_Studio_Code => Visual_Studio_Code,
       Malef.Colors.Windows_10_Console => Windows_10_Console,
       Malef.Colors.Terminal_App       => Terminal_App,
       Malef.Colors.PuTTY              => PuTTY,
       Malef.Colors.mIRC               => mIRC,
       Malef.Colors.xterm              => xterm,
       Malef.Colors.Ubuntu             => Ubuntu);

   paletteKindC2Ada : constant array (Palette_Kind'Range)
                               of Malef.Colors.Palette_Kind :=
      (Malef_Palette      => Malef.Colors.Malef_Palette,
       VGA                => Malef.Colors.VGA,
       Windows_XP_Console => Malef.Colors.Windows_XP_Console,
       Windows_PowerShell => Malef.Colors.Windows_PowerShell,
       Visual_Studio_Code => Malef.Colors.Visual_Studio_Code,
       Windows_10_Console => Malef.Colors.Windows_10_Console,
       Terminal_App       => Malef.Colors.Terminal_App,
       PuTTY              => Malef.Colors.PuTTY,
       mIRC               => Malef.Colors.mIRC,
       xterm              => Malef.Colors.xterm,
       Ubuntu             => Malef.Colors.Ubuntu);


   function To_Ada (Item : Palette_Kind)
                    return Malef.Colors.Palette_Kind is
   begin

      return paletteKindC2Ada(Item);

   end To_Ada;


   function To_C (Item : Malef.Colors.Palette_Kind)
                  return Palette_Kind is
   begin

      return paletteKindAda2C(Item);

   end To_C;



   colorKindAda2C : constant array (Malef.Colors.Color_Kind'Range)
                             of Color_Kind :=
      (Malef.Colors.Black   => Black,
       Malef.Colors.Red     => Red,
       Malef.Colors.Green   => Green,
       Malef.Colors.Yellow  => Yellow,
       Malef.Colors.Blue    => Blue,
       Malef.Colors.Magenta => Magenta,
       Malef.Colors.Cyan    => Cyan,
       Malef.Colors.White   => White);

   colorKindC2Ada : constant array (Color_Kind'Range)
                             of Malef.Colors.Color_Kind :=
      (Black   => Malef.Colors.Black,
       Red     => Malef.Colors.Red,
       Green   => Malef.Colors.Green,
       Yellow  => Malef.Colors.Yellow,
       Blue    => Malef.Colors.Blue,
       Magenta => Malef.Colors.Magenta,
       Cyan    => Malef.Colors.Cyan,
       White   => Malef.Colors.White);

   function To_Ada (Item : Color_Kind)
                    return Malef.Colors.Color_Kind is
   begin

      return colorKindC2Ada(Item);

   end To_Ada;


   function To_C (Item : Malef.Colors.Color_Kind)
                    return Color_Kind is
   begin

      return colorKindAda2C(Item);

   end To_C;


end C_Malef.Colors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
