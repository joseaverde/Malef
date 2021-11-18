-------------------------------------------------------------------------------
--                                                                           --
--                             C L O C K . A D B                             --
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

with Ada.Calendar;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

with Malef;
with Malef.Boxes;
with Malef.Characters;
with Malef.Colors;
with Malef.Surfaces;
with Malef.Windows;

procedure Clock is
   use type Malef.Row_Type;
   use type Malef.Col_Type;

   subtype Decimal is Float;
   package Math is new Ada.Numerics.Generic_Elementary_Functions (Decimal);

   Pi : constant := 3.1415_926535;

   -- The clock is formed out of 6 surfaces:
   --    1. The clock frame
   --    2. The clock background
   --    3. The clock numbers
   --    4. The hours hand
   --    5. The minutes hand
   --    6. The seconds hand
   Height : constant Malef.Row_Type := 24;
   Width  : constant Malef.Col_Type := Malef.Col_Type(Height) * 2;

   Blue  : constant Malef.Color_Type := (0, 100, 200, 255);
   White : constant Malef.Color_Type := (200, 200, 200, 255);
   Black : constant Malef.Color_Type := (56, 56, 56, 255);

   Clock_Box : Malef.Boxes.Box_Type;

   Hours_Hand   : Malef.Surfaces.Surface_Type;
   Minutes_Hand : Malef.Surfaces.Surface_Type;
   Seconds_Hand : Malef.Surfaces.Surface_Type;

   function Get_Position (Angle : Decimal; Length : Natural)
      return Malef.Cursor_Type
   is
      Fixed_Angle : constant Decimal := Angle - Pi*2.0/4.0;
      use Malef;
   begin

      return Cursor : Cursor_Type
      do
         Cursor.Row := Row_Type(Decimal'Rounding(
            Decimal(Length) * Math.Sin(Fixed_Angle) + Decimal(Height/2)));
         Cursor.Col := 2*Col_Type(Decimal'Rounding(
            Decimal(Length) * Math.Cos(Fixed_Angle) + Decimal(Height/2)));
      end return;

   end Get_Position;


   procedure Create_Clock is
      -- This function is here to prove that you don't need to keep a reference
      -- on the surfaces directly. They can be stored in a Box which frees them
      -- when it's finalised.
      --
      -- In fact the following surfaces will be used just once because they
      -- won't be changing any time soon. The Hands are the only movable
      -- objects here.
      Frame      : Malef.Surfaces.Surface_Type;
      Background : Malef.Surfaces.Surface_Type;
      Numbers    : Malef.Surfaces.Surface_Type;

      Half  : constant Decimal := Decimal(Height/2);

      Number_Images : constant array (1 .. 12) of Malef.Str_Type(1 .. 2) := (
         1  => Malef.Characters.From_Utf8("1 "),
         2  => Malef.Characters.From_Utf8("2 "),
         3  => Malef.Characters.From_Utf8("3 "),
         4  => Malef.Characters.From_Utf8("4 "),
         5  => Malef.Characters.From_Utf8("5 "),
         6  => Malef.Characters.From_Utf8("6 "),
         7  => Malef.Characters.From_Utf8("7 "),
         8  => Malef.Characters.From_Utf8("8 "),
         9  => Malef.Characters.From_Utf8("9 "),
         10 => Malef.Characters.From_Utf8("10"),
         11 => Malef.Characters.From_Utf8("11"),
         12 => Malef.Characters.From_Utf8("12")
      );

      Distance : Natural;
      Position : Malef.Cursor_Type;
      use Malef;
   begin

      -- Prepare it --
      Frame      := Malef.Surfaces.Create (Height, Width);
      Background := Malef.Surfaces.Create (Height, Width);
      Numbers    := Malef.Surfaces.Create (Height, Width);

      Frame.Clear;
      Background.Clear;
      Numbers.Clear;

      -- Frame and Background --
      for Row in Row_Type range 1 .. Height loop
         for Col in Col_Type range 1 .. Width / 2 loop
            Distance := Natural(Math.Sqrt(
               (Decimal(Row)-Half)**2 +
               (Decimal(Col)-Half)**2))+1;
            if Distance = Natural(Height) / 2 then
               Malef.Colors.Set_Background(Frame, Row, Col*2, Blue);
               Malef.Colors.Set_Background(Frame, Row, Col*2-1, Blue);
            end if;
            if Distance <= Natural(Height) / 2 then
               Malef.Colors.Set_Background(Background, Row, Col*2, White);
               Malef.Colors.Set_Background(Background, Row, Col*2-1, White);
            end if;
         end loop;
      end loop;
      Malef.Colors.Set_Background(Background, Height/2, Width/2, Black);
      Malef.Colors.Set_Background(Background, Height/2, Width/2-1, Black);

      -- Numbers --
      Malef.Colors.Set_Cursor_Background(Numbers, White);
      Malef.Colors.Set_Cursor_Foreground(Numbers, Black);
      for N in Integer range Number_Images'Range loop
         Position := Get_Position (
            Angle  => Decimal(N mod 12) / 12.0 * 2.0 * Pi,
            Length => Natural(Height/2)-3
         );
         if N mod 12 <= 6 then
            Position.Col := Position.Col - 1;
         end if;
         Numbers.Put(Number_Images(N), Position);
      end loop;


      -- Wrap Up --
      Clock_Box.Insert (Background.Get_Reference, 1);
      Clock_Box.Insert (Frame.Get_Reference, 2);
      Clock_Box.Insert (Numbers.Get_Reference, 3);
      Clock_Box.Update;

   end Create_Clock;


   procedure Create_Hands is
   begin
      Hours_Hand   := Malef.Surfaces.Create(Height, Width);
      Minutes_Hand := Malef.Surfaces.Create(Height, Width);
      Seconds_Hand := Malef.Surfaces.Create(Height, Width);

      Clock_Box.Insert (Hours_Hand.Get_Reference, 4);
      Clock_Box.Insert (Minutes_Hand.Get_Reference, 5);
      Clock_Box.Insert (Seconds_Hand.Get_Reference, 6);

      Malef.Colors.Set_Cursor_Background (Hours_Hand, Black);
      Malef.Colors.Set_Cursor_Background (Minutes_Hand, Black);
      Malef.Colors.Set_Cursor_Background (Seconds_Hand, Black);

      Malef.Colors.Set_Cursor_Foreground (Hours_Hand, Black);
      Malef.Colors.Set_Cursor_Foreground (Minutes_Hand, Black);
      Malef.Colors.Set_Cursor_Foreground (Seconds_Hand, Black);
   end Create_Hands;


   Hours_Hand_Length : constant Natural := Natural(Float(Height)/2.0 * 0.3);
   Minutes_Hand_Length : constant Natural := Natural(Float(Height)/2.0 * 0.5);
   Seconds_Hand_Length : constant Natural := Natural(Float(Height)/2.0 * 0.7);
   Hash : constant Malef.Str_Type := Malef.Characters.From_Utf8 ("#");
   procedure Update_Hands is
      Now      : constant Natural :=
         Natural(Ada.Calendar.Seconds(Ada.Calendar.Clock));

      Seconds  : constant Decimal := Decimal(Now mod 60);
      Minutes  : constant Decimal := Decimal((Now / 60) mod 60) +
         Seconds / 60.0 ;
      Hours    : constant Decimal := Decimal((Now / 3600) mod 12) +
         Minutes / 60.0 ;

      Seconds_Angle : constant Decimal := Seconds/60.0 * 2.0*Pi;
      Minutes_Angle : constant Decimal := Minutes/60.0 * 2.0*Pi;
      Hours_Angle   : constant Decimal := Hours/12.0 * 2.0*Pi;

      Position : Malef.Cursor_Type;
   begin
      -- We clear the surfaces.
      Hours_Hand.Clear;
      Minutes_Hand.Clear;
      Seconds_Hand.Clear;

      -- We build them.
      for H in Natural range 1 .. Hours_Hand_Length loop
         Position := Get_Position (
            Angle  => Hours_Angle,
            Length => H
         );
         Hours_Hand.Put (Hash, Position);
         Hours_Hand.Put (Hash, (Position.Row, Position.Col-1));
      end loop;

      for M in Natural range 1 .. Minutes_Hand_Length loop
         Position := Get_Position (
            Angle  => Minutes_Angle,
            Length => M
         );
         Minutes_Hand.Put (Hash, Position);
         Minutes_Hand.Put (Hash, (Position.Row, Position.Col+1));
      end loop;

      for S in Natural range 1 .. Seconds_Hand_Length loop
         Position := Get_Position (
            Angle  => Seconds_Angle,
            Length => S
         );
         Seconds_Hand.Put (Hash, Position);
         Seconds_Hand.Put (Hash, (Position.Row, Position.Col+1));
      end loop;
   end Update_Hands;



   function Ready_To_Quit return Boolean is
      Available : Boolean;
      Char      : Character;
   begin

      loop
         Ada.Text_IO.Get_Immediate(Char, Available);
         exit when not Available;
         if Char = 'q' or Char = 'Q' then
            return True;
         end if;
      end loop;

      return False;
   end Ready_To_Quit;


begin

   Create_Hands;
   Create_Clock;

   Malef.Initialize;
   Malef.Windows.Main_Window.Insert (Clock_Box.Get_Reference, 1);
   Malef.Windows.Main_Window.Update;

   Main_Loop: loop
      Clock_Box.Clear;
      Update_Hands;
      Clock_Box.Update;
      Malef.Windows.Main_Window.Update;
      Malef.Windows.Main_Window.Draw;

      delay 1.0;
      exit when Ready_To_Quit;
   end loop Main_Loop;

   Malef.Finalize;

end Clock;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
