-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - C O L O R S . A D B                      --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                                  B O D Y                                  --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2020-2021 José Antonio Verde Jiménez  All Rights Reserved  --
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
with Malef.Surfaces;


package body Malef.Colors is


   --============-----------------------============--
   --============-- COLOUR OPERATIONS --============--
   --============-----------------------============--


   --====----------------====--
   --====-- GET COLOUR --====--
   --====----------------====--


   procedure Get_Foreground (Surface : Malef.Surfaces.Surface_Type;
      Row   :     Row_Type;
      Col   :     Col_Type;
      Color : out Color_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      if Row > Reference.Height or Col > Reference.Width then
         raise Malef.Exceptions.Bounds_Error with
         "Cannot get color from the given position, it's out of bounds!";
      end if;

      Color := Reference.Grid(Row, Col).Format.Foreground_Color;

   end Get_Foreground;


   procedure Get_Background (Surface : Malef.Surfaces.Surface_Type;
      Row   :     Row_Type;
      Col   :     Col_Type;
      Color : out Color_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      if Row > Reference.Height or Col > Reference.Width then
         raise Malef.Exceptions.Bounds_Error with
         "Cannot get color from the given position, it's out of bounds!";
      end if;

      Color := Reference.Grid(Row, Col).Format.Background_Color;

   end Get_Background;


   function Get_Foreground (Surface : Malef.Surfaces.Surface_Type;
      Row : Row_Type;
      Col : Col_Type)
      return Color_Type
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      if Row > Reference.Height or Col > Reference.Width then
         raise Malef.Exceptions.Bounds_Error with
         "Cannot get color from the given position, it's out of bounds!";
      end if;

      return Reference.Grid(Row, Col).Format.Foreground_Color;

   end Get_Foreground;


   function Get_Background (Surface : Malef.Surfaces.Surface_Type;
      Row     : Row_Type;
      Col     : Col_Type)
      return Color_Type
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      if Row > Reference.Height or Col > Reference.Width then
         raise Malef.Exceptions.Bounds_Error with
         "Cannot get color from the given position, it's out of bounds!";
      end if;

      return Reference.Grid(Row, Col).Format.Background_Color;

   end Get_Background;


   --====----------------====--
   --====-- SET COLOUR --====--
   --====----------------====--

   procedure Set_Foreground (Surface : Malef.Surfaces.Surface_Type;
      Row   : Row_Type;
      Col   : Col_Type;
      Color : Color_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      if Reference = Shared_Null_Surface'Access then
         raise Malef.Exceptions.Null_Surface_Error with
         "Cannot change foreground colour from a Null Surface!";
      end if;

      if Row > Reference.Height or Col > Reference.Width then
         raise Malef.Exceptions.Bounds_Error with
         "Cannot change the colour of the given position, it's out of bounds!";
      end if;

      Reference.Grid(Row, Col).Format.Foreground_Color := Color;

   end Set_Foreground;


   procedure Set_Background (Surface : Malef.Surfaces.Surface_Type;
      Row   : Row_Type;
      Col   : Col_Type;
      Color : Color_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      if Reference = Shared_Null_Surface'Access then
         raise Malef.Exceptions.Null_Surface_Error with
         "Cannot change background colour from a Null Surface!";
      end if;

      if Row > Reference.Height or Col > Reference.Width then
         raise Malef.Exceptions.Bounds_Error with
         "Cannot change the colour of the given position, it's out of bounds!";
      end if;

      Reference.Grid(Row, Col).Format.Background_Color := Color;

   end Set_Background;


   procedure Set_Foreground (Surface : Malef.Surfaces.Surface_Type;
      From_Row : Row_Type;
      To_Row   : Row_Type;
      From_Col : Col_Type;
      To_Col   : Col_Type;
      Color    : Color_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      if Reference = Shared_Null_Surface'Access then
         raise Malef.Exceptions.Null_Surface_Error with
         "Cannot change the foreground colour from a Null Surface!";
      end if;

      if From_Row > Reference.Height or From_Col > Reference.Width then
         raise Malef.Exceptions.Bounds_Error with
         "Cannot change the colour of the given block because the `FROM' " &
         "position is out of bounds!";
      end if;

      if To_Row > Reference.Height or To_Col > Reference.Width then
         raise Malef.Exceptions.Bounds_Error with
         "Cannot change the colour of the given block because the `TO' " &
         "position is out of bounds!";
      end if;

      if To_Row < From_Row or To_Col < From_Col then
         raise Malef.Exceptions.Null_Surface_Error with
         "Cannot change the colours because the range is invalid!";
      end if;

      for Row in Row_Type range From_Row .. To_Row loop
         for Col in Col_Type range From_Col .. To_Col loop
            Reference.Grid(Row, Col).Format.Foreground_Color := Color;
         end loop;
      end loop;

   end Set_Foreground;


   procedure Set_Background (Surface : Malef.Surfaces.Surface_Type;
      From_Row : Row_Type;
      To_Row   : Row_Type;
      From_Col : Col_Type;
      To_Col   : Col_Type;
      Color    : Color_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      if Reference = Shared_Null_Surface'Access then
         raise Malef.Exceptions.Null_Surface_Error with
         "Cannot change the background colour from a Null Surface!";
      end if;

      if From_Row > Reference.Height or From_Col > Reference.Width then
         raise Malef.Exceptions.Bounds_Error with
         "Cannot change the colour of the given block because the `FROM' " &
         "position is out of bounds!";
      end if;

      if To_Row > Reference.Height or To_Col > Reference.Width then
         raise Malef.Exceptions.Bounds_Error with
         "Cannot change the colour of the given block because the `TO' " &
         "position is out of bounds!";
      end if;

      if To_Row < From_Row or To_Col < From_Col then
         raise Malef.Exceptions.Null_Surface_Error with
         "Cannot change the colours because the range is invalid!";
      end if;

      for Row in Row_Type range From_Row .. To_Row loop
         for Col in Col_Type range From_Col .. To_Col loop
            Reference.Grid(Row, Col).Format.Background_Color := Color;
         end loop;
      end loop;

   end Set_Background;


   procedure Set_Foreground (Surface : Malef.Surfaces.Surface_Type;
      Color   : Color_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      if Reference = Shared_Null_Surface'Access then
         raise Malef.Exceptions.Null_Surface_Error with
         "Cannot change the foreground colour from a Null Surface!";
      end if;

      for Row in Reference.Grid'Range(1) loop
         for Col in Reference.Grid'Range(2) loop
            Reference.Grid(Row, Col).Format.Foreground_Color := Color;
         end loop;
      end loop;

   end Set_Foreground;


   procedure Set_Background (Surface : Malef.Surfaces.Surface_Type;
      Color   : Color_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      if Reference = Shared_Null_Surface'Access then
         raise Malef.Exceptions.Null_Surface_Error with
         "Cannot change the background colour from a Null Surface!";
      end if;

      for Row in Reference.Grid'Range(1) loop
         for Col in Reference.Grid'Range(2) loop
            Reference.Grid(Row, Col).Format.Background_Color := Color;
         end loop;
      end loop;

   end Set_Background;



   --============------------------------------============--
   --============-- CURSOR COLOUR OPERATIONS --============--
   --============------------------------------============--

   --====----------------====--
   --====-- GET COLOUR --====--
   --====----------------====--

   procedure Get_Cursor_Foreground (Surface : Malef.Surfaces.Surface_Type;
      Color : out Color_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      Color := Reference.Cursor_Format.Foreground_Color;

   end Get_Cursor_Foreground;


   procedure Get_Cursor_Background (Surface : Malef.Surfaces.Surface_Type;
      Color : out Color_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      Color := Reference.Cursor_Format.Background_Color;

   end Get_Cursor_Background;


   function Get_Cursor_Foreground (Surface : Malef.Surfaces.Surface_Type)
      return Color_Type is
      (Surface.Get_Reference.Reference.Cursor_Format.Foreground_Color);


   function Get_Cursor_Background (Surface : Malef.Surfaces.Surface_Type)
      return Color_Type is
      (Surface.Get_Reference.Reference.Cursor_Format.Background_Color);


   --====----------------====--
   --====-- SET COLOUR --====--
   --====----------------====--

   procedure Set_Cursor_Foreground (Surface : Malef.Surfaces.Surface_Type;
      Color : Color_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      Reference.Cursor_Format.Foreground_Color := Color;

   end Set_Cursor_Foreground;


   procedure Set_Cursor_Background (Surface : Malef.Surfaces.Surface_Type;
      Color : Color_Type)
   is
      Reference : constant Shared_Surface_Access := Surface.Get_Reference.
         Reference;
   begin

      Reference.Cursor_Format.Background_Color := Color;

   end Set_Cursor_Background;



   --============-----------------------============--
   --============-- COLOUR OPERATIONS --============--
   --============-----------------------============--


   --====-----------------====--
   --====-- GET PALETTE --====--
   --====-----------------====--

   procedure Get_Palette (Palette : out Palette_Type) is
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The library hasn't been initialized yet, couldn't retrieve the " &
         "Palette!";
      end if;

      Palette := Current_Palette;

   end Get_Palette;


   function Get_Palette return Palette_Type is
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The library hasn't been initialized yet, couldn't retrieve the " &
         "Palette!";
      end if;

      return Current_Palette;

   end Get_Palette;



   procedure Set_Palette (Palette : in Palette_Type) is
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The library hasn't been initialized yet, cannot change the Palette!";
      end if;

      Shared_Null_Surface.Grid(1, 1).
         Format.Foreground_Color := Palette(False, Black);
      Shared_Null_Surface.Grid(1, 1).
         Format.Background_Color := Palette(False, White);
      Default_Element.Format.Foreground_Color := Palette (False, White);
      Default_Element.Format.Background_Color := Palette (False, Black);
      Default_Element.Format.Background_Color(A) := 0;

      Current_Palette := Palette;

   end Set_Palette;


   procedure Set_Palette (Kind : Palette_Kind) is
   begin

      if not Has_Been_Initialized then
         raise Malef.Exceptions.Initialization_Error with
         "The library hasn't been initialized yet, cannot change the Palette!";
      end if;

      Current_Palette := Palettes(Kind);

      Shared_Null_Surface.Grid(1, 1).
         Format.Foreground_Color := Current_Palette(False, Black);
      Shared_Null_Surface.Grid(1, 1).
         Format.Background_Color := Current_Palette(False, White);
      Default_Element.Format.Foreground_Color := Current_Palette(False, White);
      Default_Element.Format.Background_Color := Current_Palette(False, Black);
      Default_Element.Format.Background_Color(A) := 0;

   end Set_Palette;



   function Get_Color (
      Kind   : Color_Kind;
      Bright : Boolean := False)
      return Color_Type is
   begin

      return Current_Palette (Bright, Kind);

   end Get_Color;

end Malef.Colors;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
