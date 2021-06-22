-------------------------------------------------------------------------------
--                                                                           --
--                     M A L E F - W I N D O W S . A D B                     --
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

with Malef.Exceptions;
with Malef.Systems;

package body Malef.Windows is

   protected body Window_Type is

      procedure Create (
         Height : Row_Type;
         Width  : Col_Type) is
      begin

         if ID /= -1 then
            raise Malef.Exceptions.Window_Error
            with "Window already created!";
         end if;

         if Last_ID = 0 then
            -- Main surface.
            ID := 0;
         else
            ID := Last_ID;
            Resizing := True;
            Box.Resize (Height, Width);
            Resizing := False;
         end if;

         Box.Set_Resizable (False);
         Last_ID := Last_ID + 1;

         Updating := False;
         Drawing := False;
         Resizing := False;
         Created := True;

      end Create;


      function Contains (
         Layer : Malef.Boxes.Layer_Type)
         return Boolean is
      begin

         if not Created then
            raise Malef.Exceptions.Window_Error
            with "Window not initialized!";
         end if;
         return Box.Contains (Layer);

      end Contains;


      entry Draw
         when not Updating and not Resizing
      is
         use Malef.Systems;
      begin

         Check_Created;
         Drawing := True;
         Loaded_Subsystems (Current_Subsystem).Put
            (Box.Get_Reference.Reference);
         Drawing := False;

      end Draw;


      function Get_Position
         return Coord_Type is
      begin

         if not Created then
            raise Malef.Exceptions.Window_Error
            with "Window not initialized!";
         end if;
         return Box.Get_Reference.Reference.Position;

      end Get_Position;


      procedure Insert (
         Item  : Surface_Reference;
         Layer : Malef.Boxes.Layer_Type) is
      begin

         Check_Created;
         Box.Insert (Item, Layer);

      end Insert;


      procedure Move (
         Position : Coord_Type) is
      begin

         Check_Created;
         Box.Get_Reference.Reference.Position := Position;

      end Move;


      procedure Remove (
         Layer : Malef.Boxes.Layer_Type) is
      begin

         Check_Created;
         Box.Remove (Layer);

      end Remove;


      procedure Replace (
         Item  : Surface_Reference;
         Layer : Malef.Boxes.Layer_Type) is
      begin

         Check_Created;
         Box.Replace (Item, Layer);

      end Replace;


      procedure Resize (
         Height : Row_Type;
         Width : Col_Type) is
      begin

         Check_Created;
         if ID = 0 then
            if Height /= Malef.Height or Width /= Malef.Width then
               raise Malef.Exceptions.Window_Error
               with "Can't resize the Main Window!";
            end if;
         end if;
         Resizing := True;
         Box.Resize (Height, Width);
         Resizing := False;

      end Resize;


      entry Update
         when not Drawing and not Resizing is
      begin

         Check_Created;
         Updating := True;
         Box.Clear;
         Box.Update;
         Updating := False;

      end Update;

   -------------
   -- PRIVATE --
   -------------

      procedure Check_Created is
      begin
         if not Created then
            raise Malef.Exceptions.Window_Error
            with "Window not initialized!";
         end if;
      end Check_Created;

   end Window_Type;

begin

   Main_Window.Create (Height, Width);

end Malef.Windows;

---=======================-------------------------=========================---
--=======================-- E N D   O F   F I L E --=========================--
---=======================-------------------------=========================---
