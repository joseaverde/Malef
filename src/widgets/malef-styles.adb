-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - S T Y L E S . A D B                      --
--                                                                           --
--                                 M A L E F                                 --
--                                                                           --
--                              A D A   B O D Y                              --
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

with Ada.Characters.Conversions;

package body Malef.Styles is

   Default : constant Style := (others => <>);

   --<<--------->>--
   -->> Aspects <<--
   --<<--------->>--

   -->> Put Image <<--

   procedure Put (
      Buffer  : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Name    : in     String;
      Nothing : in     Boolean;
      Arg     : in     String) is
   begin
      if not Nothing then
         Buffer.Put (Name);
         Buffer.Put (": ");
         Buffer.Put (Arg);
         Buffer.Put (";");
         Buffer.New_Line;
      end if;
   end Put;

   procedure Put (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Name   : in     String;
      Arg    : in     Maybe_Colour) is
   begin
      Put (Buffer, Name, Arg.Nothing, Arg.Just'Image);
   end Put;

   procedure Put (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Name   : in     String;
      Arg    : in     Maybe_Border_Style) is
   begin
      Put (Buffer, Name, Arg.Nothing, Arg.Just'Image);
   end Put;

   procedure Put (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Name   : in     String;
      Arg    : in     Maybe_Border_Thickness) is
   begin
      Put (Buffer, Name, Arg.Nothing, Arg.Just'Image);
   end Put;

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Style) is
   begin
      Buffer.Put ("{");
      Buffer.New_Line;
      Buffer.Increase_Indent;
      -- General
         Put (Buffer, "foreground",          Arg.Foreground);
         Put (Buffer, "background",          Arg.Background);
      -- Borders
         Put (Buffer, "border-foreground",   Arg.Border_Foreground);
         Put (Buffer, "border-background",   Arg.Border_Background);
         Put (Buffer, "border-style",        Arg.Border_Style);
         Put (Buffer, "border-thickness",    Arg.Border_Thickness);
      -- Text
         Put (Buffer, "text-background",     Arg.Text_Background);
         Put (Buffer, "text-foreground",     Arg.Text_Foreground);
      Buffer.Decrease_Indent;
      Buffer.Put ("}");
   end Put_Image;

   -->> Set <<--

   function Wide_Wide (Item : in String)
      return Wide_Wide_String
      renames Ada.Characters.Conversions.To_Wide_Wide_String;

   function Get (
      Value : in String)
      return Maybe_Colour is (
      (if Value = "nothing"
         then (Nothing => True,
               others  => <>)
         else (Nothing => False,
               Just    => Malef.Value (Wide_Wide (Value)))));

   function Get (
      Value : in String)
      return Maybe_Border_Thickness is (
      (if Value = "nothing"
         then (Nothing => True,
               others  => <>)
         else (Nothing => False,
               Just    => Border_Thickness_Kind'Value (Value))));

   function Get (
      Value : in String)
      return Maybe_Border_Style is (
      (if Value = "nothing"
         then (Nothing => True,
               others  => <>)
         else (Nothing => False,
               Just    => Border_Style_Kind'Value (Value))));

   procedure Set (
      Object : in out Style;
      Field  : in     Style_Field;
      Value  : in     String) is
   begin
      case Field is
         -- General
         when Foreground        => Object.Foreground        := Get (Value);
         when Background        => Object.Background        := Get (Value);
         -- Borders
         when Border_Foreground => Object.Border_Foreground := Get (Value);
         when Border_Background => Object.Border_Background := Get (Value);
         when Border_Style      => Object.Border_Style      := Get (Value);
         when Border_Thickness  => Object.Border_Thickness  := Get (Value);
         -- Text
         when Text_Background   => Object.Text_Background   := Get (Value);
         when Text_Foreground   => Object.Text_Foreground   := Get (Value);
      end case;
   end Set;

   --<<--------->>--
   -->> Setters <<--
   --<<--------->>--

   -->> General <<--

   procedure Set_Background (
      Object : in out Style;
      Value  : in     RGBA_Type) is
   begin
      Object.Background := (Nothing => False, Just => Value);
   end Set_Background;

   procedure Unset_Background (
      Object : in out Style) is
   begin
      Object.Background := Default.Background;
   end Unset_Background;

   procedure Set_Foreground (
      Object : in out Style;
      Value  : in     RGBA_Type) is
   begin
      Object.Foreground := (Nothing => False, Just => Value);
   end Set_Foreground;

   procedure Unset_Foreground (
      Object : in out Style) is
   begin
      Object.Foreground := Default.Foreground;
   end Unset_Foreground;

   -->> Borders <<--

   procedure Set_Border_Foreground (
      Object : in out Style;
      Value  : in     RGBA_Type) is
   begin
      Object.Border_Foreground := (Nothing => False, Just => Value);
   end Set_Border_Foreground;

   procedure Unset_Border_Foreground (
      Object : in out Style) is
   begin
      Object.Border_Foreground := Default.Border_Foreground;
   end Unset_Border_Foreground;

   procedure Set_Border_Background (
      Object : in out Style;
      Value  : in     RGBA_Type) is
   begin
      Object.Border_Background := (Nothing => False, Just => Value);
   end Set_Border_Background;

   procedure Unset_Border_Background (
      Object : in out Style) is
   begin
      Object.Border_Background := Default.Border_Background;
   end Unset_Border_Background;

   procedure Set_Border_Style (
      Object : in out Style;
      Value  : in     Border_Style_Kind) is
   begin
      Object.Border_Style := (Nothing => False, Just => Value);
   end Set_Border_Style;

   procedure Unset_Border_Style (
      Object : in out Style) is
   begin
      Object.Border_Style := Default.Border_Style;
   end Unset_Border_Style;

   procedure Set_Border_Thickness (
      Object : in out Style;
      Value  : in     Border_Thickness_Kind) is
   begin
      Object.Border_Thickness := (Nothing => False, Just => Value);
   end Set_Border_Thickness;

   procedure Unset_Border_Thickness (
      Object : in out Style) is
   begin
      Object.Border_Thickness := Default.Border_Thickness;
   end Unset_Border_Thickness;

   -->> Text <<--

   procedure Set_Text_Background (
      Object : in out Style;
      Value  : in     RGBA_Type) is
   begin
      Object.Text_Background := (Nothing => False, Just => Value);
   end Set_Text_Background;

   procedure Unset_Text_Background (
      Object : in out Style) is
   begin
      Object.Text_Background := Default.Text_Background;
   end Unset_Text_Background;

   procedure Set_Text_Foreground (
      Object : in out Style;
      Value  : in     RGBA_Type) is
   begin
      Object.Text_Foreground := (Nothing => False, Just => Value);
   end Set_Text_Foreground;

   procedure Unset_Text_Foreground (
      Object : in out Style) is
   begin
      Object.Text_Foreground := Default.Text_Foreground;
   end Unset_Text_Foreground;

end Malef.Styles;
