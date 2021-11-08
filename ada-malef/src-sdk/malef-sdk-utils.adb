-------------------------------------------------------------------------------
--                                                                           --
--                   M A L E F - S D K - U T I L S . A D B                   --
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

with Ada.Containers.Vectors;
with Ada.Text_IO;

package body Malef.SDK.Utils is

   Space    : constant Char_Type := Char_Type(Character'Pos(' '));
   New_Line : constant Char_Type := Char_Type(Character'Pos(ASCII.LF));

   procedure Put (Surface : in out Malef.Surfaces.Surface_Type;
      Message     : Str_Type;
      Row         : Row_Type;
      Row_Count   : Row_Type;
      Col         : Col_Type;
      Col_Count   : Col_Type;
      H_Alignment : Horizontal_Alignment_Type := Align_Left;
      V_Alignment : Vertical_Alignment_Type := Align_Top)
   is
      -- To align the text we will divide the process in three parts:
      --  1. Split the message in words and arrange them so each line contains
      --  the maximum amount of words it can fit. When we split them we won't
      --  allocate new strings we will just take the ranges where each word is
      --  found.
      --  2. We put the text taking into account the alignment.

      -- Each pair represents the bounds of a string.
      type Pair_Type is array (1 .. 2) of Integer;
      package Pair_Vectors is new Ada.Containers.Vectors (
         Index_Type   => Positive,
         Element_Type => Pair_Type
      );
      subtype Pair_Vector is Pair_Vectors.Vector;

      type Line_Type is
         record
            -- We precalculate the length and the amount of pairs so we don't
            -- have to calculate them later on. The length contains the amount
            -- of spaces.
            Length : Integer := 0;
            Count  : Integer := 0;
            Pairs  : Pair_Vector;
         end record;
      package Line_Vectors is new Ada.Containers.Vectors (
         Index_Type   => Row_Type,
         Element_Type => Line_Type
      );

      Length : constant Integer := Integer(Col_Count);
      Lines : Line_Vectors.Vector;
   begin

      -----------
      -- SPLIT --
      -----------
      -- The first step is to split the message in words. If the alignment is
      -- justified we will remove extra spaces, otherwise the spaces will
      -- belong to the word itself.
      --
      -- The process is simple, we will be moving a pointer around the message
      -- and every time we find a space we will try to add it to the current
      -- line.
      Split :
         declare
            Line : Line_Type := Line_Type'(others => <>);
            From : Natural := Message'First;
            To   : Natural := Message'First;
            
            New_Word : Boolean := True;

            procedure Push_Line is
            begin
               if Line.Length /= 0 then
                  Lines.Append(Line);
                  Line := Line_Type'(others => <>);
               end if;
               New_Word := True;
            end Push_Line;

            procedure Push_Word is
               Len : Integer;
               procedure Trim_Word is
               begin
                  -- As it's the first word in the line we can strip the
                  -- trailing spaces.
                  while From <= To and then Message(From) = Space loop
                     From := From + 1;
                  end loop;
               end Trim_Word;
            begin
               if To = Message'Last and then (
                  Message(Message'Last) /= Space or
                  Message(Message'Last) /= New_Line)
               then
                  To := To + 1;
               end if;

               if Line.Count = 0 then
                  Trim_Word;
               end if;

               Len := To - From;

               -- We check if the word fits in the line.
               if Line.Length + Len >= Length then
                  -- If it doesn't fit we have to move it to the next line.
                  -- This function changes the Line variable and resets it.
                  Push_Line;
                  Trim_Word;
               end if;

               -- If we can't still fit the word in the line we will have to
               -- split it.
               if Line.Length + Len >= Length then
                  Line.Length := Length;
                  Line.Count := 1;
                  Line.Pairs.Append((From, From + Length - 1));
                  From := From + Length;
                  Push_Word;
               else
                  Line.Length := Integer'Min(
                     Line.Length + Len + 1,
                     Length
                  );
               end if;

               if Len = 0 then
                  return;
               end if;

               -- We can add the new word.
               Line.Count := Line.Count + 1;
               Line.Pairs.Append((From, To - 1));
               From := To + 1;
               New_Word := True;
            end Push_Word;
         begin
            while To < Message'Last loop
               -- If we find a space there are two possible outcomes if the
               -- text is justified we force a new word. Otherwise the space
               -- belongs to the second word.
               if Message(To) = Space and then
                  (H_Alignment = Justified or
                  (H_Alignment /= Justified and not New_Word))
               then
                  -- This is a new word and we can forget about the space.
                  Push_Word;
               elsif Message(To) = New_Line then
                  -- We force a new line therefore we have to append a new line
                  -- if possible.
                  Push_Word;
                  Push_Line;
               else
                  New_Word := False;
               end if;
               To := To + 1;
            end loop;
            Push_Word;
            Push_Line;
         end Split;


      -------------
      -- ARRANGE --
      -------------
      -- Once we have prepared the bounds of the different words of the text we
      -- can start adding them.
   
      -- The algorithm to align left, right and centered is the same, only
      -- changing the column it starts writing.
      Arrange :
         declare
            R : Row_Type;
            C : Col_Type;

            Line : Line_Type;
            Line_Count : constant Row_Type := Row_Type(
               Line_Vectors.Length(Lines));
            Pair : Pair_Type;

            Acc : Float := 0.0;
            Inc : Float := 0.0;
         begin
            case V_Alignment is
               when Align_Top    =>
                  R := Row;
               when Align_Bottom =>
                  if Line_Count >= Row_Count then
                     R := Row;
                  else
                     R := Row + Row_Count - Line_Count;
                  end if;
               when Centered =>
                  if Line_Count >= Row_Count then
                     R := Row;
                  else
                     R := Row + (1 + Row_Count - Line_Count) / 2;
                  end if;
            end case;
            for I in 1 .. Line_Count loop
               Ada.Text_Io.PUt_Line(R'Image);
               Line := Lines(I);
               case H_Alignment is
                  when Align_Left =>
                     C := Col;
                  when Align_Right =>
                     C := Col + Col_Count - Col_Type(Line.Length) + 1;
                  when Centered   =>
                     C := Col + (Col_Count - Col_Type(Line.Length) + 1) / 2;
                  when Justified =>
                     C := Col;
                     if Line.Count > 1 then
                        Acc := 0.0;
                        Inc := Float(Col_Count - Col_Type(Line.Length) + 1) /
                           Float(Line.Count - 1);
                     end if;
               end case;
               Surface.Set_Cursor_Position (
                  Row => R,
                  Col => C
               );
               for P in Integer range 1 .. Line.Count loop
                  Pair := Line.Pairs(P);
                  C := C + Col_Type(Pair(2)) - Col_Type(Pair(1)) + 1;
                  Surface.Put(Message(Pair(1) .. Pair(2)));
                  if C < Col + Col_Count then
                     Surface.Put(Space);
                     C := C + 1;
                  end if;

                  if H_Alignment = Justified and then (
                     (I /= Line_Count and then Lines(I+1).Count /= 0))
                  then
                     Acc := Acc + Inc;
                     if P /= Line.Count then
                        while Float'Rounding(Acc) > 0.0 loop
                           Acc := Acc - 1.0;
                           C := C + 1;
                           Surface.Put(Space);
                        end loop;
                     end if;
                  end if;
               end loop;
               R := R + 1;
               exit when R >= Row + Row_Count;
            end loop;
         end Arrange;

   end Put;

end Malef.SDK.Utils;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
