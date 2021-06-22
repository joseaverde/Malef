-------------------------------------------------------------------------------
--                                                                           --
--             M A L E F - S U B S Y S T E M S - A N S I . A D B             --
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

with Ada.Finalization;
with Malef.Colors;
with Malef.Systems;
with Malef.Systems.Utils; use Malef.Systems.Utils;

package body Malef.Subsystems.Ansi is

   type Subsystem_Controller is new Ada.Finalization.Limited_Controlled
      with null record;
   overriding procedure Initialize (SC : in out Subsystem_Controller);
   overriding procedure Finalize   (SC : in out Subsystem_Controller);

   Subsystem_Handler : aliased Subsystem;


   overriding
   procedure New_Page (Subsys : not null access Subsystem) is
      New_Lines : constant String (1 .. Integer(Height)) :=
         (others => Character'Val(10));
   begin
      Std_Out.Write (New_Lines);
      Std_Out.Dump;
   end New_Page;


   overriding
   procedure Set_Title (Subsys : not null access Subsystem;
      Title  : String) is
   begin
      Std_Out.Write (ASCII.ESC & "]2;" & Title & Character'Val(16#07#));
      Std_Out.Dump;
   end Set_Title;


   overriding
   procedure Clear_Screen (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[J");
      Std_Out.Dump;

   end Clear_Screen;


   overriding
   procedure Clear_Until_End_Of_Screen (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[0J");
      Std_Out.Dump;

   end Clear_Until_End_Of_Screen;


   overriding
   procedure Clear_Until_Start_Of_Screen (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[1J");
      Std_Out.Dump;

   end Clear_Until_Start_Of_Screen;


   overriding
   procedure Clear_Entire_Screen (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[2J");
      Std_Out.Dump;

   end Clear_Entire_Screen;



   overriding
   procedure Clear_Current_Line (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[K");
      Std_Out.Dump;

   end Clear_Current_Line;


   overriding
   procedure Clear_Until_End_Of_Line (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[0K");
      Std_Out.Dump;

   end Clear_Until_End_Of_Line;


   overriding
   procedure Clear_Until_Start_Of_Line (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[1K");
      Std_Out.Dump;

   end Clear_Until_Start_Of_Line;


   overriding
   procedure Clear_Entire_Line (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[2K");
      Std_Out.Dump;

   end Clear_Entire_Line;


   
   overriding
   procedure Enable_Line_Wrapping (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[=7h");
      Std_Out.Dump;

   end Enable_Line_Wrapping;


   overriding
   procedure Disable_Line_Wrapping (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[=7l");
      Std_Out.Dump;

   end Disable_Line_Wrapping;


   
   overriding
   procedure Make_Cursor_Visible (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[?25h");
      Std_Out.Dump;

   end Make_Cursor_Visible;


   overriding
   procedure Make_Cursor_Invisible (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[?25l");
      Std_Out.Dump;

   end Make_Cursor_Invisible;


   
   overriding
   procedure Save_Screen (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[?47h");
      Std_Out.Dump;

   end Save_Screen;


   overriding
   procedure Restore_Screen (Subsys : not null access Subsystem) is
   begin

      Std_Out.Write (ASCII.ESC & "[?47l");
      Std_Out.Dump;

   end Restore_Screen;



   Lock : Boolean := True;
   overriding
   procedure Put (Subsys : not null access Subsystem;
      Object : Shared_Surface_Access)
   is
      Last_Format : Format_Type := Default_Format;

      -- The position of the surface on the screen.
      In_Row     : Row_Type;
      In_Col     : Col_Type;
      -- Position to start iterating from.
      From_Row   : Row_Type;
      From_Col   : Col_Type;
      -- How much to iterate.
      The_Height : Row_Type;
      The_Width  : Col_Type;

   begin

      -- We first define the bounds and calculate the offset to print it.
      if Object.Position.Row + Row_Coord (Object.Height) < 1 or   -- Too up
         Object.Position.Col + Col_Coord (Object.Width)  < 1 or   -- Too left
         abs Object.Position.Row > Row_Coord (Height)        or   -- Too down
         abs Object.Position.Col > Col_Coord (Width)              -- Too right
      then
         -- Nothing to be done.
         return;
      end if;

      -- Otherwise it's safe to define the bounds.
      -- If it starts out of bounds we let it start at the begining, we've
      -- already checked the bounds.
      In_Row := Row_Type
         (if Object.Position.Row < 1 then 1 else Object.Position.Row);
      In_Col := Col_Type
         (if Object.Position.Col < 1 then 1 else Object.Position.Col);
      From_Row := Row_Type
         (if Object.Position.Row < 0 then abs Object.Position.Row else 1);
      From_Col := Col_Type
         (if Object.Position.Col < 0 then abs Object.Position.Col else 1);
      The_Height := Row_Type'Min
         (Height, Row_Type(Row_Coord(Object.Height) + Object.Position.Row))-1;
      The_Width := Col_Type'Min
         (Width, Col_Type(Col_Coord(Object.Width) + Object.Position.Col))-1;

      -- We wait until the operation is unlocked because we can't put two
      -- surfaces onto the screen at the same time, because they may interfere.
      -- This is done so it's safe for multitasking.
      while Lock loop null; end loop;
      Lock := True;

      -- We start by clearing the format to a default one, it will probably
      -- be changed, but it doesn't matter.
      Std_Out.Write (Get_Format(Last_Format));
      for Row in Row_Type range From_Row .. From_Row + The_Height - 1 loop
         Std_Out.Write (Get_Move (In_Row + Row, In_Col));
         for Col in Col_Type range From_Col .. From_Col + The_Width - 1 loop
            -- Everytime the format changes, we have to clear the last one in
            -- order to set the new one. This is due to some bugs with styles.
            -- For example, to add the Bold effect we use 'ESC[1m', and to
            -- remove it we have to prepend a '2': 'ESC[21m'. However, in some
            -- terminals, this also meas Doble-Underline. So to make sure
            -- everything is correct we have to clear everything everytime a
            -- new format is set.
            if Last_Format /= Object.Grid (Row, Col).Format then
               Std_Out.Write (Get_Clear);
               Last_Format := Object.Grid (Row, Col).Format;
               Std_Out.Write (Get_Format (Last_Format));
            end if;

            -- Once the format is set we proceed to write the character.
            case Object.Grid (Row, Col).Char is
               when 0 .. 31 | 127 =>
                  -- Non printable characters.
                  -- TODO: Special treatment
                  Std_Out.Write (Get_Move (In_Row + Row - 1, In_Col + Col));
               when others =>
                  -- We just write them.
                  Std_Out.Write (Object.Grid (Row, Col).Char);
            end case;
         end loop;
      end loop;

      -- We dump the buffer and unlock it.
      Std_Out.Write (Get_Clear);
      Std_Out.Dump;
      Lock := False;

   end Put;


   function Get_Color_1 (
      Foreground : Color_Type;
      Background : Color_Type)
      return String is
   begin

      return "";

   end Get_Color_1;


   function Get_Color_3 (
      Foreground : Color_Type;
      Background : Color_Type)
      return String
   is
      Pal  : constant Malef.Colors.Palette_Type :=
         Malef.Colors.Get_Palette;
      Fg_Diff : Integer := 0;
      Bg_Diff : Integer := 0;
      Fg_Min  : Natural := Natural'Last;
      Bg_Min  : Natural := Natural'Last;
      Col     : Color_Type;

      Fg_Kind : Malef.Colors.Color_Kind;
      Bg_Kind : Malef.Colors.Color_Kind;

      Colors : array (Malef.Colors.Color_Kind'Range) of Character :=
         "01234567";
   begin

      for Color in Pal'Range(2) loop
         Col := Pal (False, Color);
         Fg_Diff := abs ( Integer(Foreground(R)) - Integer(Col(R)) ) +
                    abs ( Integer(Foreground(G)) - Integer(Col(G)) ) +
                    abs ( Integer(Foreground(B)) - Integer(Col(B)) ) ;
         Bg_Diff := abs ( Integer(Background(R)) - Integer(Col(R)) ) +
                    abs ( Integer(Background(G)) - Integer(Col(G)) ) +
                    abs ( Integer(Background(B)) - Integer(Col(B)) ) ;
         if Fg_Diff < Fg_Min then
            Fg_Min    := Fg_Diff;
            Fg_Kind   := Color;
         end if;
         if Bg_Diff < Bg_Min then
            Bg_Min    := Bg_Diff;
            Bg_Kind   := Color;
         end if;
      end loop;

      return ASCII.ESC & '[' &
               '3' & Colors(Fg_Kind) & ';' &
               '4' & Colors(Bg_Kind) & 'm';

   end Get_Color_3;


   function Get_Color_4 (
      Foreground : Color_Type;
      Background : Color_Type)
      return String
   is
      Pal  : constant Malef.Colors.Palette_Type :=
         Malef.Colors.Get_Palette;
      Fg_Diff : Integer := 0;
      Bg_Diff : Integer := 0;
      Fg_Min  : Natural := Natural'Last;
      Bg_Min  : Natural := Natural'Last;
      Col     : Color_Type;

      Fg_Bright : Boolean;
      Fg_Kind   : Malef.Colors.Color_Kind;
      Bg_Bright : Boolean;
      Bg_Kind   : Malef.Colors.Color_Kind;

      Colors : array (Malef.Colors.Color_Kind'Range) of Character :=
         "01234567";
   begin

      -- We look for the minimum difference of colours with the palettes.
      for Bright in Pal'Range(1) loop
         for Color in Pal'Range(2) loop
            Col := Pal (Bright, Color);
            Fg_Diff := abs ( Integer(Foreground(R)) - Integer(Col(R)) ) +
                       abs ( Integer(Foreground(G)) - Integer(Col(G)) ) +
                       abs ( Integer(Foreground(B)) - Integer(Col(B)) ) ;
            Bg_Diff := abs ( Integer(Background(R)) - Integer(Col(R)) ) +
                       abs ( Integer(Background(G)) - Integer(Col(G)) ) +
                       abs ( Integer(Background(B)) - Integer(Col(B)) ) ;
            if Fg_Diff < Fg_Min then
               Fg_Min    := Fg_Diff;
               Fg_Bright := Bright;
               Fg_Kind   := Color;
            end if;
            if Bg_Diff < Bg_Min then
               Bg_Min    := Bg_Diff;
               Bg_Bright := Bright;
               Bg_Kind   := Color;
            end if;
         end loop;
      end loop;

      return ASCII.ESC & '[' &
               (if not Fg_Bright then "3" else "9") &
               Colors(Fg_Kind) & ';' &
               (if not Bg_Bright then "4" else "10") &
               Colors(Bg_Kind) & 'm';

   end Get_Color_4;


   function Get_Color_8 (
      Foreground : Color_Type;
      Background : Color_Type)
      return String
   is
      Raw_Fg : Color_Type;
      Raw_Bg : Color_Type;
   begin

      -- TODO: Find a fast formula.
      -- Colour = 16 + 36*R + 6*G + B
      --
      -- We first reduce the colours.
      for I in Color_Type'Range loop
         Raw_Fg (I) := Foreground (I) / 43;
         Raw_Bg (I) := Background (I) / 43;
      end loop;

      return ASCII.ESC & '[' &
               "38:5:" & To_String(16 + Raw_Fg(R)   * 36 +
                                        Raw_Fg(G) * 6  +
                                        Raw_Fg(B)) & ';' &
               "48:5:" & To_String(16 + Raw_Bg(R)   * 36 +
                                        Raw_Bg(G) * 6  +
                                        Raw_Bg(B)) &
               'm';

   end Get_Color_8;


   function Get_Color_24 (
      Foreground : Color_Type;
      Background : Color_Type)
      return String is
   begin

      return ASCII.ESC & '[' &
               "38;2;" & To_String(Foreground(R)) & ';' &
                         To_String(Foreground(G)) & ';' &
                         To_String(Foreground(B)) & ';' &
               "48;2;" & To_String(Background(R)) & ';' &
                         To_String(Background(G)) & ';' &
                         To_String(Background(B)) &
               'm';

   end Get_Color_24;


   function Get_Style (Style : Style_Array)
      return String is
   begin

      -- TODO
      return "";

   end Get_Style;


   function Get_Move (
      Row : Row_Type;
      Col : Col_Type)
      return String is
   begin

      return ASCII.ESC & "[" &
                  To_String(Positive(Row)) & ';' &
                  To_String(Positive(Col)) &
             'H';

   end Get_Move;


   function Get_Format (
      Format : Format_Type)
      return String is
   begin

      return Get_Color.all (
               Foreground => Format.Foreground_Color,
               Background => Format.Background_Color
             ) &
             Get_Style (Format.Styles);

   end Get_Format;


   function Get_Clear return String is
   begin

      return ASCII.ESC & "[0m";

   end Get_Clear;



   overriding
   procedure Initialize (SC : in out Subsystem_Controller) is
      use Malef.Systems;
   begin

      Malef.Systems.Loaded_Subsystems(Malef.ANSI) :=
         Subsystem_Handler'Access;

      -- We get the best colour function.
      for Bit in reverse Malef.Systems.Color_Bits'Range loop
         if Malef.Systems.Available_Colors (Bit) then
            case Bit is
               when Bit24  => Get_Color := Get_Color_24'Access; exit;
               when Bit8   => Get_Color := Get_Color_8 'Access; exit;
               when Bit4   => Get_Color := Get_Color_4 'Access; exit;
               when Bit3   => Get_Color := Get_Color_3 'Access; exit;
               when others => Get_Color := Get_Color_1 'Access; exit;
            end case;
         end if;
      end loop;

      Lock := False;

   end Initialize;


   overriding
   procedure Finalize (SC : in out Subsystem_Controller) is
   begin

      Malef.Systems.Loaded_Subsystems(Malef.ANSI) := null;
      Get_Color := Get_Color_1'Access;

      Lock := True;

   end Finalize;

   pragma Warnings (Off);
   SC : Subsystem_Controller;
   pragma Warnings (On);

end Malef.Subsystems.Ansi;

 
---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
