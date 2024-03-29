-------------------------------------------------------------------------------
--                                                                           --
--              M A L E F - S T Y L E S - C L A S S E S . A D B              --
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

package body Malef.Styles.Classes is

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Style_Class) is
   begin
      Buffer.Put ("""");
      if not Arg.Classes.Is_Empty then
         Buffer.Wide_Wide_Put (Arg.Classes.First_Element);
         for I in Arg.Classes.First_Index + 1 .. Arg.Classes.Last_Index loop
            Buffer.Put (" ");
            Buffer.Wide_Wide_Put (Arg.Classes (I));
         end loop;
      end if;
      Buffer.Put ("""");
   end Put_Image;

   function Value (
      Item : in Wide_Wide_String)
      return Style_Class
   is
      First : Positive := Item'First;
      Last  : Positive := Item'Last;
   begin
      return Class : Style_Class do
         while First in Item'Range loop
            while First in Item'Range and then Item (First) = ' ' loop
               First := First + 1;
            end loop;
            Last := First;
            while Last in Item'Range and then Item (Last) /= ' ' loop
               Last := Last + 1;
            end loop;

            if First in Item'Range then
               if Last in Item'Range then
                  Class.Classes.Append (Item (First .. Last));
               else
                  Class.Classes.Append (Item (First .. Last - 1));
               end if;
            end if;
            First := Last + 1;
         end loop;
      end return;
   end Value;

   type Iterator (Class : not null access constant Style_Class) is
      limited new Style_Class_Iterator_Interfaces.Forward_Iterator with
      null record;

   overriding
   function First (
      Object : in Iterator)
      return Cursor is (
      Cursor (Object.Class.Classes.First));

   overriding
   function Next (
      Object   : in Iterator;
      Position : in Cursor)
      return Cursor is (
      Cursor (Class_Vectors.Next (Class_Vectors.Cursor (Position))));

   function Iterate (
      Item : aliased in Style_Class)
      return Style_Class_Iterator_Interfaces.Forward_Iterator'Class is (
      Iterator'(Class => Item'Access));

end Malef.Styles.Classes;
