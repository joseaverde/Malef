-------------------------------------------------------------------------------
--                                                                           --
--              M A L E F - S U B S Y S T E M S - C M D . A D B              --
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
with Interfaces.C;
with Malef.Subsystems.Components.Colors;
with Malef.Subsystems.Components.Put_Utils;
with Malef.Systems;

package body Malef.Subsystems.CMD is

   type Subsystem_Controller is new Ada.Finalization.Limited_Controlled
      with null record;
   overriding procedure Initialize (SC : in out Subsystem_Controller);
   overriding procedure Finalize   (SC : in out Subsystem_Controller);

   Subsystem_Handler : aliased Subsystem;


   overriding
   procedure New_Page (Subsys : not null access Subsystem) is
   begin
      for I in 1 .. Height loop
         Std_Out.Write (ASCII.CR & ASCII.LF);
      end loop;
      Std_Out.Dump;
   end New_Page;


   overriding
   procedure Set_Title (Subsys : not null access Subsystem;
      Title  : String)
   is
      procedure C_Driver_Set_Console_Title (Title : Interfaces.C.Char_Array)
         with Import        => True,
              Convention    => C,
              External_Name => "_malefCMD_setConsoleTitle";
   begin

      C_Driver_Set_Console_Title (Interfaces.C.To_C(Title));

   end Set_Title;



   overriding
   procedure Clear_Screen (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Clear_Screen;


   overriding
   procedure Clear_Until_End_Of_Screen (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Clear_Until_End_Of_Screen;


   overriding
   procedure Clear_Until_Start_Of_Screen (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Clear_Until_Start_Of_Screen;


   overriding
   procedure Clear_Entire_Screen (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Clear_Entire_Screen;



   overriding
   procedure Clear_Current_Line (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Clear_Current_Line;


   overriding
   procedure Clear_Until_End_Of_Line (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Clear_Until_End_Of_Line;


   overriding
   procedure Clear_Until_Start_Of_Line (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Clear_Until_Start_Of_Line;


   overriding
   procedure Clear_Entire_Line (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Clear_Entire_Line;


   
   overriding
   procedure Enable_Line_Wrapping (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Enable_Line_Wrapping;


   overriding
   procedure Disable_Line_Wrapping (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Disable_Line_Wrapping;


   
   overriding
   procedure Make_Cursor_Visible (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Make_Cursor_Visible;


   overriding
   procedure Make_Cursor_Invisible (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Make_Cursor_Invisible;


   
   overriding
   procedure Save_Screen (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Save_Screen;


   overriding
   procedure Restore_Screen (Subsys : not null access Subsystem) is
   begin raise Program_Error with "Not Implemented";
   end Restore_Screen;



   Lock : Boolean := True;
   overriding
   procedure Put (Subsys : not null access Subsystem;
      Object : Shared_Surface_Access)
   is
      Last_Format : Format_Type := Default_Format;
      In_Row      : Row_Type;
      In_Col      : Col_Type;
      From_Row    : Row_Type;
      From_Col    : Col_Type;
      The_Height  : Row_Type;
      The_Width   : Col_Type;
   begin

      Malef.Subsystems.Components.Put_Utils.Get_Bounds (Object => Object,
         In_Row     => In_Row,
         In_Col     => In_Col,
         From_Row   => From_Row,
         From_Col   => From_Col,
         The_Height => The_Height,
         The_Width  => The_Width
      );

      while Lock loop null; end loop; Lock := True;
      Malef.Subsystems.Components.Colors.Precalculate;

      Set_Format (Last_Format);
      for Row in Row_Type range From_Row .. From_Row + The_Height -  1 loop
         Set_Position ((In_Row + Row, In_Col));
         for Col in Col_Type range From_Col .. From_Col + The_Width - 1 loop
            if Last_Format /= Object.Grid (Row, Col).Format then
               Last_Format := Object.Grid (Row, Col).Format;
               Set_Format (Last_Format);
            end if;

            case Object.Grid (Row, Col).Char is
               when 0 .. 31 | 127 =>
                  -- TODO: Keep in mind attributes
                  -- TODO: Special treatment
                  Set_Position ((In_Row + Row - 1, In_Col + Col));
               when others =>
                  Std_Out.Write (Object.Grid (Row, Col).Char);
            end case;
         end loop;
      end loop;

      Std_Out.Dump;
      Lock := False;

   exception
      when Malef.Subsystems.Components.Put_Utils.Pass => return;
   end Put;



   overriding
   procedure Initialize (SC : in out Subsystem_Controller) is
      procedure C_Driver_Initialize
         with Import        => True,
              Convention    => C,
              External_Name => "_malefCMD_initialize";
   begin

      Malef.Systems.Loaded_Subsystems (Malef.CMD) :=
         Subsystem_Handler'Access;
      C_Driver_Initialize;
      Lock := False;

   end Initialize;


   overriding
   procedure Finalize (SC : in out Subsystem_Controller) is
   begin

      Malef.Systems.Loaded_Subsystems (Malef.CMD) := null;
      Lock := True;

   end Finalize;

   pragma Warnings (Off);
   SC : Subsystem_Controller;
   pragma Warnings (On);


   ----- PRIVATE -----

   procedure Set_Format (Format : Format_Type) is
      use type WORD;
      procedure C_Driver_Set_Format (format : WORD)
         with Import        => True,
              Convention    => C,
              External_Name => "_malefCMD_setFormat";
      Win_Format : WORD := ATTRIBUTE_ZERO;
      BG_HSB_Color, FG_HSB_Color : Malef.Subsystems.Components.Colors.HSB_Type;
      BG_Bright, FG_Bright : Boolean;
      BG_Color, FG_Color : Color_Kind;
      BG_Brightness, FG_Brightness : Integer;
      To_Be_Underlined : constant array (Positive range <>) of Style_Type :=
         (Italic, Underline, Conceal, Doubly_Underline);
   begin

      Std_Out.Dump;
      -- To get the correct format we have to keep in mind many factors. Such
      -- as Colour HUE, brightness and styles:
      --
      -- * Bold, Faint and Original Brightness
      -- * Italic, Underline, Conceal, Doubly_Underline
      -- * Rapid_Blink and Slow_Blink
      -- * Crossed_Out
      -- * Reverse_Video
      --
      -- We first get the colour Hue and Brightness
      --
      BG_HSB_Color := Malef.Subsystems.Components.Colors.To_HSB (
         Format.Background_Color
      );
      FG_HSB_Color := Malef.Subsystems.Components.Colors.To_HSB (
         Format.Foreground_Color
      );
      Malef.Subsystems.Components.Colors.To_Color_4 (BG_HSB_Color,
         Out_Color  => BG_Color,
         Brightness => BG_Bright
      );
      Malef.Subsystems.Components.Colors.To_Color_4 (FG_HSB_Color,
         Out_Color  => FG_Color,
         Brightness => FG_Bright
      );

      -- The HUEness has already been decided we can add it to the format.
      -- We can also tell if it's reversed or not.
      Win_Format := Windows_Conversion (FG_Color) or
                    Windows_Conversion (BG_Color) * 16 or
                   (if Format.Styles(Reverse_Video)
                    then COMMON_LVB_REVERSE_VIDEO
                    else ATTRIBUTE_ZERO);
      -- We can underline if any of Italic, Underline, Conceal or
      -- Doubly_Underline is currently in use.
      for Style of To_Be_Underlined loop
         if Format.Styles (Style) then
            Win_Format := Win_Format or COMMON_LVB_UNDERSCORE;
            exit;
         end if;
      end loop;

      -- Finally we weight the brightness
      BG_Brightness := (if BG_Bright then 1 else 0);
      FG_Brightness := (if FG_Bright then 1 else 0);
      if Format.Styles (Bold) then
         BG_Brightness := BG_Brightness + 1;
         FG_Brightness := FG_Brightness + 1;
      end if;
      if Format.Styles (Faint) then
         BG_Brightness := BG_Brightness - 1;
         FG_Brightness := FG_Brightness - 1;
      end if;

      Win_Format := Win_Format or
       (if BG_Brightness <= 0 then BACKGROUND_INTENSITY else ATTRIBUTE_ZERO) or
       (if FG_Brightness <= 0 then FOREGROUND_INTENSITY else ATTRIBUTE_ZERO);

      C_Driver_Set_Format (Win_Format);

   end Set_Format;


   procedure Set_Position (Position : Cursor_Type) is
      procedure C_Driver_Set_Position (Row, Col : WORD)
         with Import        => True,
              Convention    => C,
              External_Name => "_malefCMD_setPosition";
   begin

      Std_Out.Dump;
      C_Driver_Set_Position (WORD (Position.Row), WORD (Position.Col));

   end Set_Position;


end Malef.Subsystems.CMD;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
