-------------------------------------------------------------------------------
--                                                                           --
--   MALEF - S U B S Y S T E M S - C O M P O N E N T S - C O L O R S . ADB   --
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

with Malef.Colors; use Malef.Colors;

package body Malef.Subsystems.Components.Colors is

   function To_String (
      C : Color_Component_Type)
      return String is
   begin

      return Color_Components_Strings(C);

   end To_String;


   function To_String (
      N : Natural)
      return String
   is
      Size : Natural := 1;
   begin

      -- We can speed up the process a little bit, because most terminals
      -- won't exceed the 255 rows or columns. Therefore we can make use of
      -- the other To_String function.
      if N < 256 then
         return Color_Components_Strings(Color_Component_Type(N));
      else
         -- Otherwise, the usual procedure.
         while N / 10 ** Size /= 0 loop
            Size := Size + 1;
         end loop;

         Create_String:
            declare
               Str : String (1 .. Size);
            begin
               for I in Natural range 1 .. Size loop
                  Str (Size + 1 - I) := Character'Val (
                     (N / 10**(I - 1)) mod 10 + 48);
               end loop;

               return Str;
            end Create_String;
      end if;

   end To_String;



   procedure Precalculate is
   begin

      -- We update the palette if only it had changed.
      if Current_Palette /= Get_Palette then
         Current_Palette := Get_Palette;
         for Bright in Current_Palette'Range(1) loop
            for Color in Current_Palette'Range(2) loop
               The_Palette (Bright, Color) :=
                  To_HSB (Current_Palette(Bright, Color));
            end loop;
         end loop;
      end if;

   end Precalculate;


   procedure To_Color_1 (Color : in HSB_Type;
      Is_White : out Boolean) is
   begin
      Is_White := Color.Brightness > 0.5;
   end To_Color_1;


   procedure To_Color_3 (Color : in HSB_Type;
      Out_Color : out Color_Kind)
   is
      Best  : Percentage := Percentage'First;
      Match : Percentage;
   begin

      for Col in The_Palette'Range (2) loop
         Match := Color_Match (The_Palette(False, Col), Color);
         if Match > Best then
            Best := Match;
            Out_Color := Col;
         end if;
      end loop;

   end To_Color_3;


   procedure To_Color_4 (Color : HSB_Type;
      Out_Color  : out Color_Kind;
      Brightness : out Boolean)
   is
      Best  : Percentage := Percentage'First;
      Match : Percentage;
   begin

      for Bright in The_Palette'Range (1) loop
         for Col in The_Palette'Range (2) loop
            Match := Color_Match (The_Palette(Bright, Col), Color);
            if Match > Best then
               Best := Match;
               Out_Color := Col;
               Brightness := Bright;
            end if;
         end loop;
      end loop;

   end To_Color_4;


   procedure To_Color_8 (Color : Color_Type;
      Out_Color : out Color_Component_Type) is
   begin

      -- Colour = 16 + 36*R + 6*G + B
      -- (We reduce the colour by dividing it by 43)
      Out_Color := 16 +          -- OFFSET
         Color(R) / 43 * 36 +    -- RED
         Color(G) / 43 * 6  +    -- GREEN
         Color(B) / 43;          -- BLUE

   end To_Color_8;



   -- Formula taken from here
   --    https://www.cs.rit.edu/~ncs/color/t_convert.html
   function To_HSB (Color : Color_Type)
      return HSB_Type
   is
      Red   : constant Float := Float (Color(R)) / 255.0;
      Green : constant Float := Float (Color(G)) / 255.0;
      Blue  : constant Float := Float (Color(B)) / 255.0;
      Max   : constant Float := Float'Max (Float'Max (Red, Green), Blue);
      Min   : constant Float := Float'Min (Float'Min (Red, Green), Blue);
      Delt  : constant Float := Max - Min;
      Hue   : Float;
   begin

      return HSB : HSB_Type
      do
         HSB.Brightness := SB_Type (Max);

         if Max /= 0.0 then
            HSB.Saturation := SB_Type (Delt / Max);

            if Red = Max then
               Hue := (Green - Blue) / Delt;
            elsif Green = Max then
               Hue := 2.0 + (Blue - Red) / Delt;
            else
               Hue := 4.0 + (Red - Green) / Delt;
            end if;
            Hue := Hue * 60.0;
            if Hue < 0.0 then
               Hue := Hue + 360.0;
            end if;
         else
            -- Brightness is undefined
            HSB.Saturation := 0.0;
            HSB.Hue := 0.0;
         end if;
      end return;

   end To_HSB;



   function To_RGB (Color : HSB_Type)
      return Color_Type is
   begin

      -- TODO
      raise Program_Error with "Not Implemented!";
      return (0, 0, 0, 0);

   end To_RGB;


   function Color_Match (Left, Right : HSB_Type)
      return Percentage
   is
      Hue_Diff        : constant Float := abs
         (Float(Left.Hue) - Float(Right.Hue));
      Saturation_Diff : constant Float := abs
         (Float(Left.Saturation) - Float(Right.Saturation));
      Brightness_Diff : constant Float := abs
         (Float(Left.Brightness) - Float(Right.Brightness));
   begin
      return Percentage (1.0 -
            (Hue_Diff / 360.0 + Saturation_Diff + Brightness_Diff) / 3.0);
   exception
      when Constraint_Error =>
         -- TODO: Fix this. When running clock.exe a constraint error is
         -- raised here.
         return 0.0;
   end Color_Match;


end Malef.Subsystems.Components.Colors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
