-------------------------------------------------------------------------------
--                                                                           --
--           M A L E F - S D K - M E S S A G E _ B O X E S . A D B           --
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

with Ada.Text_IO;

with Malef.Characters;
with Malef.Colors;

with Malef.SDK.Utils;


package body Malef.SDK.Message_Boxes is

   procedure Load_Indexes is
      use type Malef.Boxes.Layer_Type;
      I : Malef.Boxes.Layer_Type := 3;
   begin
      for Enum in Indexes'Range loop
         Indexes(Enum) := I;
         I := I + 1;
      end loop;
   end Load_Indexes;


   function Create (
      Message          : Str_Type;
      Height           : Row_Type;
      Width            : Col_Type;
      Foreground_Color : Color_Type;
      Background_Color : Color_Type;
      Selected_Color   : Color_Type;
      Shadow_Color     : Color_Type := Default_Shadow_Color;
      Borders          : String := "+-+| |+-+";
      H_Alignment      : Horizontal_Alignment_Type := Align_Left;
      V_Alignment      : Vertical_Alignment_Type := Centered)
      return Message_Box_Type
   is
      Message_Box : Message_Box_Type := Message_Box_Type'(
         Message_Boxes_Widgets.Widget_Type with
         Message => new Str_Type'(Message),
         others  => <>
      );
      Surface : Malef.Surfaces.Surface_Type;
      Frame   : constant Str_Type (1 .. 9) :=
         Malef.Characters.From_UTF8(Malef.Characters.UTF8_String(Borders));
   begin

      -- We create Surface and the Shadow.
      Message_Box.Surface := Malef.Surfaces.Create (Height, Width);
      Message_Box.Shadow  := Malef.Surfaces.Create (Height, Width);
      Malef.Colors.Set_Cursor_Foreground(Message_Box.Surface,Foreground_Color);
      Malef.Colors.Set_Cursor_Background(Message_Box.Surface,Background_Color);
      Malef.Colors.Set_Background (Message_Box.Shadow, Shadow_Color);

      Message_Box.Box.Insert (Message_Box.Surface.Get_Reference, 2);
      Message_Box.Box.Insert (Message_Box.Shadow.Get_Reference, 1);

      Message_Box.Shadow.Set_Position (2, 3);

      Surface := Message_Box.Surface;

      Surface.Set_Cursor_Position (1, 1);
      Surface.Put (Frame(1));
      for C in Col_Type range 2 .. Width - 1 loop
         Surface.Put (Frame(2));
      end loop;
      Surface.Put (Frame(3));

      for R in Row_Type range 2 .. Height - 1 loop
         Surface.Set_Cursor_Position (R, 1);
         Surface.Put (Frame(4));
         for C in Col_Type range 2 .. Width - 1 loop
            Surface.Put (Frame(5));
         end loop;
         Surface.Put (Frame(6));
      end loop;

      Surface.Set_Cursor_Position (Height, 1);
      Surface.Put (Frame(9));
      for C in Col_Type range 2 .. Width - 1 loop
         Surface.Put (Frame(8));
      end loop;
      Surface.Put (Frame(9));
      Message_Box.Surface.Set_Cursor_Position (1, 1);


      -- Once the message bubble is created is time to create the buttons.
      -- The buttons are individual surfaces stored in the box in order from
      -- index 3.
      Load_Indexes;
      declare
         Temp     : Malef.Surfaces.Surface_Type;
         Lengths  : array (Return_Type'Range) of Natural;
         Max      : Integer := 0;
         Sum      : Integer := 0;
         I        : Natural := 1;
      begin
         for Elem in Return_Type'Range loop
            Lengths(Elem) := Image(Elem)'Length;
            if Lengths(Elem) > Max then
               Max := Lengths(Elem);
            end if;
            Sum := Sum + Lengths(Elem);
            Temp := Malef.Surfaces.Create (1, Col_Type(Lengths(Elem)));
            Message_Box.Box.Insert (Temp.Get_Reference, Indexes(Elem));
            if Elem = Return_Type'First then
               Malef.Colors.Set_Cursor_Background (Temp, Selected_Color);
            else
               Malef.Colors.Set_Cursor_Background (Temp, Background_Color);
            end if;
            Malef.Colors.Set_Cursor_Foreground (Temp, Foreground_Color);
            Temp.Put(Malef.Characters.From_UTF8(
               Malef.Characters.UTF8_String(Image(Elem))));
            Message_Box.Buttons(Elem) := Temp;
         end loop;
         -- Once we have prepared every element, it's time to calculate their
         -- positions.
         -- We use the biggest length as a reference for how many items
         -- there should be in each part.
         -- Horizontal holds the amount of buttons that fit in one line.
         Message_Box.Horizontal := (Natural(Width) - 2) / (Max+1);
         Message_Box.Vertical :=
            Indexes'Length / Message_Box.Horizontal +
            (if Indexes'Length mod Message_Box.Horizontal = 0 then 0 else 1);

         -- Once we have calculated the amount of items per row (horizontal)
         -- and the amount of rows (vertical), we can move the buttons.
         -- Sum variable will hold from now on the amount of items that we have
         -- to move at the end which are left.
         Sum := Indexes'Length mod Message_Box.Horizontal;
         for Elem in Return_Type'Range loop
            Temp := Message_Box.Buttons(Elem);
            Temp.Set_Position ((
               -- Row Position:
               Row => Row_Coord (
                  Natural(Height) - Message_Box.Vertical +
                  I / Message_Box.Horizontal
               ),

               -- Column Position:
               Col => Col_Coord (
                  -- We need to align it.
                  2 + 
                  (
                  if I >= Indexes'Length - Sum then
                     (I mod Message_Box.Horizontal) * 
                     Natural(Width - 2) / (1 + Sum) 
                  else
                     (Max+1) * (I mod Message_Box.Horizontal)
                  )
                  - Lengths(Elem) / 2 
               )
            ));
            I := I + 1;
         end loop;
      end;

      -- Finally we can call a external function to put the string into the
      -- surface with alignment.
      Malef.SDK.Utils.Put (Surface,
         Message     => Message,
         Row         => 2,
         Row_Count   => Height - Row_Type(Message_Box.Vertical) - 3,
         Col         => 3,
         Col_Count   => Width - 4,
         H_Alignment => H_Alignment,
         V_Alignment => V_Alignment
      );

      Message_Box.Update(False);

      return Message_Box;

   end Create;


   overriding
   function Get_Reference (Message_Box : in Message_Box_Type)
      return Surface_Reference is
   begin

      return Message_Box.Box.Get_Reference;

   end Get_Reference;

   overriding
   function Run (Message_Box : in out Message_Box_Type)
      return Return_Type
   is
      Buffer : Character;
   begin

      pragma Unreferenced (Message_Box);
      -- TODO: Use library functions
      loop
         Ada.Text_IO.Get_Immediate(Buffer);
         exit when Character'Pos(Buffer) = 10;
      end loop;

      return Return_Type'First;

   end Run;

   overriding
   procedure Update (Message_Box : in out Message_Box_Type;
      Focused : Boolean) is
   begin

      Message_Box.Box.Update(Focused);

   end Update;


   overriding
   procedure Finalize (Message_Box : in out Message_Box_Type) is
      Upcast : constant access Base_Type := Base_Type(Message_Box)'Access;
   begin

      Upcast.Finalize;

   end Finalize;


end Malef.SDK.Message_Boxes;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
