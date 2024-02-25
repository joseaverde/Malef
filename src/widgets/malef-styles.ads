-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - S T Y L E S . A D S                      --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   S P E C                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2021-2024 José Antonio Verde Jiménez  All Rights Reserved  --
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

package Malef.Styles with Pure is

   -- This type provides a simple way of specifying the style for your
   -- application using Ada or a CSS-like language.

   type Border_Thickness_Kind is (Invisible, Thin, Thick);
   type Border_Style_Kind is (Square, Double, Rounded);

   type Style_Field is (

      -->> General <<--

      Background, Foreground,

      -->> Borders <<--

      Border_Foreground, Border_Background, Border_Style, Border_Thickness,

      -->> Text <<--

      Text_Background, Text_Foreground);

   type Style is private with
      Put_Image => Put_Image,
      Aggregate => (Empty     => Empty,
                    Add_Named => Set);

   --<<--------->>--
   -->> Aspects <<--
   --<<--------->>--

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Style);

   function Empty return Style;

   procedure Set (
      Object : in out Style;
      Field  : in     Style_Field;
      Value  : in     String);

   -- TODO: String_Literal

   --<<------------>>--
   -->> Operations <<--
   --<<------------>>--

   function "+" (Left, Right : in Style)
      return Style;

   --<<--------->>--
   -->> Getters <<--
   --<<--------->>--

   -->> General <<--

   function Get_Background (
      Object : in Style)
      return RGBA_Type;

   function Is_Set_Background (
      Object : in Style)
      return Boolean;

   function Get_Foreground (
      Object : in Style)
      return RGBA_Type;

   function Is_Set_Foreground (
      Object : in Style)
      return Boolean;

   -->> Borders <<--

   function Get_Border_Foreground (
      Object : in Style)
      return RGBA_Type;

   function Is_Set_Border_Foreground (
      Object : in Style)
      return Boolean;

   function Get_Border_Background (
      Object : in Style)
      return RGBA_Type;

   function Is_Set_Border_Background (
      Object : in Style)
      return Boolean;

   function Get_Border_Style (
      Object : in Style)
      return Border_Style_Kind;

   function Is_Set_Border_Style (
      Object : in Style)
      return Boolean;

   function Get_Border_Thickness (
      Object : in Style)
      return Border_Thickness_Kind;

   function Is_Set_Border_Thickness (
      Object : in Style)
      return Boolean;

   -->> Text <<--

   function Get_Text_Background (
      Object : in Style)
      return RGBA_Type;

   function Is_Set_Text_Background (
      Object : in Style)
      return Boolean;

   function Get_Text_Foreground (
      Object : in Style)
      return RGBA_Type;

   function Is_Set_Text_Foreground (
      Object : in Style)
      return Boolean;

   --<<--------->>--
   -->> Setters <<--
   --<<--------->>--

   -->> General <<--

   procedure Set_Background (
      Object : in out Style;
      Value  : in     RGBA_Type);

   procedure Unset_Background (
      Object : in out Style);

   procedure Set_Foreground (
      Object : in out Style;
      Value  : in     RGBA_Type);

   procedure Unset_Foreground (
      Object : in out Style);

   -->> Borders <<--

   procedure Set_Border_Foreground (
      Object : in out Style;
      Value  : in     RGBA_Type);

   procedure Unset_Border_Foreground (
      Object : in out Style);

   procedure Set_Border_Background (
      Object : in out Style;
      Value  : in     RGBA_Type);

   procedure Unset_Border_Background (
      Object : in out Style);

   procedure Set_Border_Style (
      Object : in out Style;
      Value  : in     Border_Style_Kind);

   procedure Unset_Border_Style (
      Object : in out Style);

   procedure Set_Border_Thickness (
      Object : in out Style;
      Value  : in     Border_Thickness_Kind);

   procedure Unset_Border_Thickness (
      Object : in out Style);

   -->> Text <<--

   procedure Set_Text_Background (
      Object : in out Style;
      Value  : in     RGBA_Type);

   procedure Unset_Text_Background (
      Object : in out Style);

   procedure Set_Text_Foreground (
      Object : in out Style;
      Value  : in     RGBA_Type);

   procedure Unset_Text_Foreground (
      Object : in out Style);

   --<<--------->>--
   -->> Streams <<--
   --<<--------->>--

   -- TODO: procedure Load (
   --          Stream : not null access Root_Stream_Type'Class;
   --          Object : in out Style);
   -- TODO: procedure Dump (
   --          Stream : not null access Root_Stream_Type'Class;
   --          Object : in out Style);
   --
   -- TODO: Subpackage Malef.Styles.Watcher that updates a style if the file
   --       has changed.

private

   type Maybe_Colour is
      record
         Nothing : Boolean   := True;
         Just    : RGBA_Type := [0, 0, 0, 0];
      end record;

   type Maybe_Border_Thickness is
      record
         Nothing : Boolean               := True;
         Just    : Border_Thickness_Kind := Thin;
      end record;

   type Maybe_Border_Style is
      record
         Nothing : Boolean           := True;
         Just    : Border_Style_Kind := Square;
      end record;

   type Style is
      record
         -->> General <<--
         Foreground        : Maybe_Colour           := (Nothing => True,
                                                        Just    => "#FFF");
         Background        : Maybe_Colour           := (Nothing => True,
                                                        Just    => "#000");
         -->> Borders <<--
         Border_Foreground : Maybe_Colour           := (Nothing => True,
                                                        Just    => "#FFF");
         Border_Background : Maybe_Colour           := (Nothing => True,
                                                        Just    => "#000");
         Border_Style      : Maybe_Border_Style     := (Nothing => True,
                                                        Just    => Square);
         Border_Thickness  : Maybe_Border_Thickness := (Nothing => True,
                                                        Just    => Thin);
         -->> Text <<--
         Text_Background   : Maybe_Colour           := (Nothing => True,
                                                        Just    => "#000");
         Text_Foreground   : Maybe_Colour           := (Nothing => True,
                                                        Just    => "#FFF");
      end record;

   --<<--------->>--
   -->> Aspects <<--
   --<<--------->>--

   function Empty return Style is (
      Style'(others => <>));

   --<<------------>>--
   -->> Operations <<--
   --<<------------>>--

   function "+" (Left, Right : in Maybe_Colour)
      return Maybe_Colour is (
      (if Left.Nothing then Right else Left));

   function "+" (Left, Right : in Maybe_Border_Style)
      return Maybe_Border_Style is (
      (if Left.Nothing then Right else Left));

   function "+" (Left, Right : in Maybe_Border_Thickness)
      return Maybe_Border_Thickness is (
      (if Left.Nothing then Right else Left));

   function "+" (Left, Right : in Style)
      return Style is (
      -- General
        Background        => Left.Background        + Right.Background
      , Foreground        => Left.Foreground        + Right.Foreground
      -->> Borders <<--
      , Border_Foreground => Left.Border_Foreground + Right.Border_Foreground
      , Border_Background => Left.Border_Background + Right.Border_Background
      , Border_Style      => Left.Border_Style      + Right.Border_Style
      , Border_Thickness  => Left.Border_Thickness  + Right.Border_Thickness
      -->> Text <<--
      , Text_Background   => Left.Text_Background   + Right.Text_Background
      , Text_Foreground   => Left.Text_Foreground   + Right.Text_Foreground
      );

   --<<--------->>--
   -->> Getters <<--
   --<<--------->>--

   -->> General <<--

   function Get_Background (
      Object : in Style)
      return RGBA_Type is (
      Object.Background.Just);

   function Is_Set_Background (
      Object : in Style)
      return Boolean is (
      not Object.Background.Nothing);

   function Get_Foreground (
      Object : in Style)
      return RGBA_Type is (
      Object.Foreground.Just);

   function Is_Set_Foreground (
      Object : in Style)
      return Boolean is (
      not Object.Foreground.Nothing);

   -->> Borders <<--

   function Get_Border_Foreground (
      Object : in Style)
      return RGBA_Type is (
      Object.Border_Foreground.Just);

   function Is_Set_Border_Foreground (
      Object : in Style)
      return Boolean is (
      not Object.Border_Foreground.Nothing);

   function Get_Border_Background (
      Object : in Style)
      return RGBA_Type is (
      Object.Border_Background.Just);

   function Is_Set_Border_Background (
      Object : in Style)
      return Boolean is (
      not Object.Border_Background.Nothing);

   function Get_Border_Style (
      Object : in Style)
      return Border_Style_Kind is (
      Object.Border_Style.Just);

   function Is_Set_Border_Style (
      Object : in Style)
      return Boolean is (
      not Object.Border_Style.Nothing);

   function Get_Border_Thickness (
      Object : in Style)
      return Border_Thickness_Kind is (
      Object.Border_Thickness.Just);

   function Is_Set_Border_Thickness (
      Object : in Style)
      return Boolean is (
      not Object.Border_Thickness.Nothing);

   -->> Text <<--

   function Get_Text_Background (
      Object : in Style)
      return RGBA_Type is (
      Object.Text_Background.Just);

   function Is_Set_Text_Background (
      Object : in Style)
      return Boolean is (
      not Object.Text_Background.Nothing);

   function Get_Text_Foreground (
      Object : in Style)
      return RGBA_Type is (
      Object.Text_Foreground.Just);

   function Is_Set_Text_Foreground (
      Object : in Style)
      return Boolean is (
      not Object.Text_Foreground.Nothing);

end Malef.Styles;
