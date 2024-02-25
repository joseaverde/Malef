-------------------------------------------------------------------------------
--                                                                           --
--                    M A L E F - D E B U G _ I O . A D B                    --
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

with Ada.Containers.Indefinite_Vectors;
with Ada.Finalization;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Strings;

package body Malef.Debug_IO is

   package WW_Unicode renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   package W_Unicode renames Ada.Strings.UTF_Encoding.Wide_Strings;

   type Debug_Entry (
      Length : Natural) is
      record
         Severity : Message_Severity;
         Message  : String (1 .. Length);
      end record;

   package Debug_Entry_Vectors is
      new Ada.Containers.Indefinite_Vectors (
      Index_Type   => Positive,
      Element_Type => Debug_Entry);

   protected type Debug_Buffer is
      procedure Put (
         Kind : in Message_Severity;
         Item : in String);
   private
      Entries : Debug_Entry_Vectors.Vector;
   end Debug_Buffer;

   protected body Debug_Buffer is
      procedure Put (
         Kind : in Message_Severity;
         Item : in String) is
      begin
         Entries.Append (Debug_Entry'(Length   => Item'Length,
                                      Severity => Kind,
                                      Message  => Item));
      end Put;
   end Debug_Buffer;

   package body Debug_IO is

      use Ada.Streams;
      use type Stream_IO.Stream_Access;

      Buffer : Debug_Buffer;
      Output : Stream_IO.File_Type;
      Stream : Stream_IO.Stream_Access := null;

      -->> Helpers <<--

      procedure Check_Open is
      begin
         if Stream /= null then
            return;
         end if;
         if not Widget then
            Stream_IO.Create (Output, Stream_IO.Out_File, File_Name);
            Stream := Stream_IO.Stream (Output);
         end if;
      end Check_Open;

      -->> Output <<--

      procedure Put (
         Kind : in Message_Severity;
         Item : in String) is
      begin
         if Mode = Release then
            return;
         end if;
         Check_Open;
         if Widget then
            Buffer.Put (Kind, Item);
         else
            String'Write (Stream, "[");
            String'Write (Stream, Kind'Image);
            String'Write (Stream, "] ");
            String'Write (Stream, Item);
            Character'Write (Stream, ASCII.LF);
         end if;
      end Put;

      procedure Wide_Put (
         Kind : in Message_Severity;
         Item : in Wide_String) is
      begin
         Put (Kind, W_Unicode.Encode (Item));
      end Wide_Put;

      procedure Wide_Wide_Put (
         Kind : in Message_Severity;
         Item : in Wide_Wide_String) is
      begin
         Put (Kind, WW_Unicode.Encode (Item));
      end Wide_Wide_Put;

      procedure Put (
         Item : in String) is
      begin
         Put (Severity, Item);
      end Put;

      procedure Wide_Put (
         Item : in Wide_String) is
      begin
         Wide_Put (Severity, Item);
      end Wide_Put;

      procedure Wide_Wide_Put (
         Item : in Wide_Wide_String) is
      begin
         Wide_Wide_Put (Severity, Item);
      end Wide_Wide_Put;

      -->> Debug Widget <<--

      overriding
      procedure On_Draw (
         Object  : in     Debug_Widget;
         Surface : in out Surfaces.Surface;
         Area    : in     Widgets.Draw_Area) is null;

      function New_Debug
         return Debug_Widget is
      begin
         return Widget : Debug_Widget;
      end New_Debug;

      -->> Allow finalization <<--

      type Handler is new Ada.Finalization.Limited_Controlled with null record;

      overriding procedure Finalize (Object : in out Handler) is
      begin
         if not Widget and then Stream /= null then
            Stream_IO.Close (Output);
         end if;
      end Finalize;

      H : Handler with Unreferenced;

   end Debug_IO;

end Malef.Debug_IO;
