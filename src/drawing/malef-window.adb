-------------------------------------------------------------------------------
--                                                                           --
--                      M A L E F - W I N D O W . A D B                      --
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
-- with this program. If not, s://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with Malef.Console_IO;
with Malef.Surfaces;

package body Malef.Window is

   procedure Show (Surface : in Surfaces.Surface) is
   begin
      Malef.Console_IO.Begin_Frame;
      for Row in 1 .. Surface.Rows loop
         for Col in 1 .. Surface.Cols loop
            Malef.Console_IO.Put (
               Position   => (Row, Col),
               Item       => Surface (Row, Col),
               Background => Surface.Get_Background (Row, Col),
               Foreground => Surface.Get_Foreground (Row, Col),
               Style      => Surface (Row, Col));
         end loop;
      end loop;
      Malef.Console_IO.End_Frame;
      Malef.Console_IO.Flush;
   end Show;

   protected body Window is

      procedure Process_Group (
         Process : not null access
                   procedure (Object : aliased in out Groups.Group)) is
      begin
         Process.all (Group.Set_Group (1).Element.all);
      end Process_Group;

      procedure Set_Group (Object : in Groups.Group) is
      begin
         Group.Insert (1, Object);
      end Set_Group;

      procedure Resize (
         Rows : in Positive_Row_Count;
         Cols : in Positive_Col_Count) is null;

      procedure Display is
      begin
         Group.Set_Group (1).Update;
         Show (Group.Get_Group (1).See_Surface.Element.all);
      end Display;

      procedure Redraw is null;

      -->> Callbacks <<--

      procedure Register (
         Event    : in Event_Name;
         Observer : not null access Event_Observer'Class;
         Callback : not null        Callback_Type) is
      begin
         Observers (Event).Append (
            Implementation.Observer_Info'(Observer, Callback));
      end Register;

      procedure Unregister (
         Event    : in Event_Name;
         Observer : not null access Event_Observer'Class) is
      begin
         Observers (Event).Delete (Index => Observers (Event).Find_Index (
            Item => Implementation.Observer_Info'(Observer, null)));
      end Unregister;

   end Window;

end Malef.Window;
