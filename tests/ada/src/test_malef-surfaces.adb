-------------------------------------------------------------------------------
--                                                                           --
--               T E S T _ M A L E F - S U R F A C E S . A D B               --
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

with Tests;

with Malef;
with Malef.Characters;
with Malef.Exceptions;
with Malef.Surfaces;

package body Test_Malef.Surfaces is

   use type Malef.Row_Type;
   use type Malef.Col_Type;
   use type Malef.Surfaces.Surface_Type;

   -- CREATE --

   Original_Surface : Malef.Surfaces.Surface_Type;
   Copied_Surface   : Malef.Surfaces.Surface_Type;
   Height : constant Malef.Row_Type := 20;
   Width  : constant Malef.Col_Type := 40;

   function Create return String is
   begin

      Original_Surface := Malef.Surfaces.Create (Height, Width);

      return "<>";

   end Create;

   FUNCTION CREATE_BIG RETURN STRING IS
      BIG_SURFACE : MALEF.SURFACES.SURFACE_TYPE;
   BEGIN

      BIG_SURFACE := MALEF.SURFACES.CREATE(
         ROWS => 512,
         COLS => 512);

      RETURN "<>";

   END CREATE_BIG;


   -- PUT --

   Original_Str : constant String := "Curse the World";
   Char : constant Malef.Char_Type := Malef.Char_Type(Character'Pos('#'));
   Str  : constant Malef.Str_Type  := Malef.Characters.From_Utf8(
      Malef.Characters.Utf8_String(Original_Str));
   Char_Position : constant Malef.Cursor_Type := (Height/2, Width/2);
   Str_Position  : constant Malef.Cursor_Type := (2, 2);
   function Put_Char return String is
   begin

      Original_Surface.Put (Char, Char_Position);

      return "<>";

   end Put_Char;


   function Put_String return String is
   begin

      Original_Surface.Put (Str, Str_Position);

      return "<>";

   end Put_String;


   function Put_Char_Exception return String is
   begin

      Original_Surface.Put (Char, (Height+1, Width+1));

      return "<>";

   exception
      when Malef.Exceptions.Bounds_Error =>
         return "Bounds_Error";
   end Put_Char_Exception;


   function Put_String_Exception return String is
   begin

      Original_Surface.Put (Str, (
         Height/2,
         Width - Str'Length + 2));

      return "<>";

   exception
      when Malef.Exceptions.Bounds_Error =>
         return "Bounds_Error";
   end Put_String_Exception;


   -- GET --

   function Get_Char return String is
      Char : Malef.Str_Type (1 .. 1);
   begin

      Char(1) := Original_Surface.Get (Char_Position);

      return String(Malef.Characters.To_Utf8 (Char));

   end Get_Char;


   function Get_String return String is
      Buffer : Malef.Str_Type (Str'Range);
   begin

      Buffer := Original_Surface.Get (Str_Position, Buffer'Length);

      return String(Malef.Characters.To_Utf8 (Buffer));

   end Get_String;


   function Get_Char_Exception return String is
      Char : Malef.Char_Type;
   begin

      Char := Original_Surface.Get ((Height+1, Width+1));

      return "<>";

   exception
      when Malef.Exceptions.Bounds_Error =>
         return "Bounds_Error";
   end Get_Char_Exception;


   function Get_String_Exception return String is
      Buffer : Malef.Str_Type (Str'Range);
   begin

      Buffer := Original_Surface.Get (
         (Height/2, Width-Str'Length+2),
         Buffer'Length
      );

      return "<>";

   exception
      when Malef.Exceptions.Bounds_Error =>
         return "Bounds_Error";
   end Get_String_Exception;


   -- COPY AND COMPARING --

   function Copy return String is
   begin

      Copied_Surface := Original_Surface.Copy;

      return "<>";

   end Copy;


   function Compare return String is
   begin

      if Copied_Surface /= Original_Surface then
         return "False";
      else
         return "True";
      end if;

   end Compare;


   -- MAIN --

   procedure Main is
   begin

      -- CREATE --
      Tests.Test (Unit     => Create'Access,
                  Name     => "Create",
                  Expected => "<>");

      TESTS.TEST (UNIT     => CREATE_BIG'ACCESS,
                  NAME     => "Create",
                  EXPECTED => "<>");

      -- PUT --
      Tests.Test (Unit     => Put_Char'Access,
                  Name     => "Put",
                  Expected => "<>");

      Tests.Test (Unit     => Put_String'Access,
                  Name     => "Put",
                  Expected => "<>");

      Tests.Test (Unit     => Put_Char_Exception'Access,
                  Name     => "Put",
                  Expected => "Bounds_Error");

      Tests.Test (Unit     => Put_String_Exception'Access,
                  Name     => "Put",
                  Expected => "Bounds_Error");


      -- GET --
      Tests.Test (Unit     => Get_Char'Access,
                  Name     => "Get",
                  Expected => "#");

      Tests.Test (Unit     => Get_String'Access,
                  Name     => "Get",
                  Expected => Original_Str);

      Tests.Test (Unit     => Get_Char_Exception'Access,
                  Name     => "Get",
                  Expected => "Bounds_Error");

      Tests.Test (Unit     => Get_String_Exception'Access,
                  Name     => "Get",
                  Expected => "Bounds_Error");


      -- COPY AND COMPARING --
      Tests.Test (Unit     => Copy'Access,
                  Name     => "Copy",
                  Expected => "<>");

      Tests.Test (Unit     => Compare'Access,
                  Name     => "Compare",
                  Expected => "True");

   end Main;

end Test_Malef.Surfaces;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
